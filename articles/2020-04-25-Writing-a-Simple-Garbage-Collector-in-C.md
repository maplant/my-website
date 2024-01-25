## Introduction 

People seem to think that writing a garbage collector is really hard, a deep magic understood by a few great sages and [Hans Boehm (et al)](https://hboehm.info/gc/index.html). Well it's not. In fact, it's rather straight forward. I claim that the hardest part in writing a GC is writing the memory allocator, which is as hard to write as it is to look up the malloc example in K&R.

A few important things to note before we begin. First, our code will be dependent on the Linux kernel. Not GNU/Linux, but the Linux kernel. Secondly, our code will be 32-bit and not one bit more. Thirdly. Please don't use this code. I did not intend for it to be wholly correct and there may be subtle bugs I did not catch. Regardless, the ideas themselves are still correct. Now, let's get started.

If you see any inaccuracies, please send me an email.

## Making the malloc

To begin, we need to write a memory allocator, or as we will be calling it, a malloc function. The simplest malloc implementations maintain a linked-list of free blocks of memory that can be partitioned and given out as needed. When a user requests a chunk of memory, a block of the right size is removed from the free list and returned. If no blocks of the right size exist, either a block of a larger size is partitioned into smaller blocks or more memory is requested from the kernel. Freeing a chunk of memory simply adds it back to the free list.

Each chunk of memory in the free list begins with a header describing the block. Our header will contain two fields, one indicating the size of the chunk and the second pointing to the next free block of memory: 

```c
typedef struct header {
    unsigned int    size;
    struct header   *next;
} header_t;
```

 Using headers that are embedded in the memory we allocate is really the only sensible way of doing this, but it has the added benefit of automatically word-aligning the chunks, which is important.

Because we will need to keep track of the blocks of memory currently in use as well as the blocks that are not, we will have a used list in addition to a free list. Items will be added to the used list when they are removed from the free list, and vice-versa.

We are almost ready to complete the first step and write our malloc implementation. Before we do that, we first need to understand how to request memory from the kernel.

Dynamically allocated memory resides in the so-called heap, a section memory between the stack and the BSS (uninitialized data segment - all your global variables that have the default value of zero). The heap starts at a low address bordering the BSS and ends at the program break, which resides somewhere between the BSS and the stack. Attempting to access any memory between the stack and the break will cause an access violation (unless you access within the amount the stack can be extended by, but that's a whole separate conversation). In order to obtain more memory from the kernel, we simply extend the break, thus allowing us to access more memory. To do this, we call the Unix sbrk system call, which extends the break by its argument and returns the address of the previous break on success, thus giving the program more memory. On failure, sbrk returns -1 casted to a void pointer, which is a terrible convention that no one likes.

We can use this knowledge to create two functions: `morecore` and `add_to_free_list`. In the case that we are out of blocks in the free list, we will call `morecore` to request more memory. Since requesting the kernel for more memory is expensive, we will do it in page-size chunks. Knowing what a page is is not important right now, but a terse explanation is that it is the smallest unit of virtual memory that can be mapped to any particular location in physical memory. We will use the function `add_to_free_list` to do exactly what it sounds like. 

```c
static header_t base;           /* Zero sized block to get us started. */
static header_t *freep = &base; /* Points to first free block of memory. */
static header_t *usedp;         /* Points to first used block of memory. */

/*
 * Scan the free list and look for a place to put the block. Basically, we're 
 * looking for any block that the to-be-freed block might have been partitioned from.
 */
static void
add_to_free_list(header_t *bp)
{
    header_t *p;

    for (p = freep; !(bp > p && bp < p->next); p = p->next)
        if (p >= p->next && (bp > p || bp < p->next))
            break;

    if (bp + bp->size == p->next) {
        bp->size += p->next->size;
        bp->next = p->next->next;
    } else
        bp->next = p->next;

    if (p + p->size == bp) {
        p->size += bp->size;
        p->next = bp->next;
    } else
        p->next = bp;

    freep = p;
}

#define MIN_ALLOC_SIZE 4096 /* We allocate blocks in page sized chunks. */

/*
 * Request more memory from the kernel.
 */
static header_t *
morecore(size_t num_units)
{
    void *vp;
    header_t *up;

    if (num_units > MIN_ALLOC_SIZE)
        num_units = MIN_ALLOC_SIZE / sizeof(header_t);

    if ((vp = sbrk(num_units * sizeof(header_t))) == (void *) -1)
        return NULL;

    up = (header_t *) vp;
    up->size = num_units;
    add_to_free_list (up);
    return freep;
}
```

Now that we have our two helper functions, writing our malloc function is pretty straight forward. We simply scan the free list and use the first block that is at least as big as the chunk we're trying to find. Because we use the first block we find instead of trying to find a "better" block, this algorithm is known as first fit.

A quick note to clarify: the size field in the header struct is measured in header-sized blocks, and not bytes. 

```c
/*
 * Find a chunk from the free list and put it in the used list.
 */
void *
GC_malloc(size_t alloc_size)
{
    size_t num_units;
    header_t *p, *prevp;

    num_units = (alloc_size + sizeof(header_t) - 1) / sizeof(header_t) + 1;  
    prevp = freep;

    for (p = prevp->next;; prevp = p, p = p->next) {
        if (p->size >= num_units) { /* Big enough. */
            if (p->size == num_units) /* Exact size. */
                prevp->next = p->next;
            else {
                p->size -= num_units;
                p += p->size;
                p->size = num_units;
            }

            freep = prevp;

            /* Add to p to the used list. */
            if (usedp == NULL)  
                usedp = p->next = p;
            else {
                p->next = usedp->next;
                usedp->next = p;
            }

            return (void *) (p + 1);
        }
        if (p == freep) { /* Not enough memory. */
            p = morecore(num_units);
            if (p == NULL) /* Request for more memory failed. */
                return NULL;
        }
    }
}
```

Although this code isn't going to win any awards for low fragmentation, it'll work. And if it works, that means we can finally get to the fun part - the garbage collection! 

## Mark and Sweep

 We did say that the garbage collector was going to be simple, so we will be using the simplest algorithm possible: stop the world naive mark and sweep. This algorithm works in two parts:

First, we scan all the blocks of memory that could possibly point to heap data and see if any do. To do this, for each word-size chunk in the memory we're looking at, we look at each block in the used list. If the word-sized chunk's value is within the range of a used block, we mark the block.

Next, after all possible memory locations have been searched, we go through the used list and add to the free list all blocks that haven't been marked.

Many people (or at least I did) get tripped up into thinking that garbage collection is impossible in C because by writing a simple function like malloc there is no way of knowing many things about the outside world. For example, there is no function in C that returns a hash map to all the variables that have been stack-allocated. But we can get by without this, by realizing two important facts:

Firstly (gosh I say that a lot), in C, you can _attempt_ to access any virtual memory address you want. There is no chunk of memory that for some reason the compiler can access but has an address that cannot be expressed as an integer and then casted to a pointer. It isn't possible. If memory is used in a C program, it can be accessed by the program. This is a confusing notion for programmers unfamiliar to C, as many languages provide restricted access to virtual memory addresses. C does not.

Secondly, all variables are stored somewhere in memory. Well duh. But what that means is that if we know generally where the variables are stored, we can look through that memory and find all the possible values of every variable. Additionally, because memory access is generally only word-aligned, we only need to look through every word in the memory regions.

Local variables can also be stored in registers, but we won't worry about this because registers and usually dedicated to local variables, and by the time our function is called they'll probably be saved on the stack anyway

Now we have a strategy for the marking phase of our collector: look through a bunch of memory regions and see if there is any memory that looks like it references something in the used list. Writing a function to do that is pretty clear cut: 

```c
#define UNTAG(p) (((uintptr_t) (p)) & 0xfffffffc)

/*
 * Scan a region of memory and mark any items in the used list appropriately.
 * Both arguments should be word aligned.
 */
static void
scan_region(uintptr_t *sp, uintptr_t *end)
{
    header_t *bp;

    for (; sp < end; sp++) {
        uintptr_t v = *sp;
        bp = usedp;
        do {
            if (bp + 1 <= v &&
                bp + 1 + bp->size > v) {
                    bp->next = ((uintptr_t) bp->next) | 1;
                    break;
            }
        } while ((bp = UNTAG(bp->next)) != usedp);
    }
}
```

In order to ensure we only use two words in the header we use a technique here called tagged pointers. Since our next pointers will be word aligned, a few of the least significant bits will always be zero. Thus, we mark the least significant bit of the next pointer to indicate that the current block (not the one pointed to by next!) has been marked.

Now we can scan memory regions, but which memory regions should we look through? There are several relevant regions:

* The BSS (uninitialized data) and the initialized data segments: These contain all the global and static variables in the program. Thus, they could reference something in our heap.
* The used chunks: Of course, if the user allocates a pointer to another allocated chunk, we don't want to free the pointed-to chunk.
* The stack: Since the stack contains all the local variables, this is arguably the most important place to look.

### Scanning the heap 

The heap is not a contiguous region of memory like the other regions. At first this seems to contradict our code; in order to allocate memory, we extend the end of the data segment with sbrk. Therefore it seems like it should be possible to scan the whole heap region as we would any other.

This isn't correct because modern memory doesn't actually more this way anymore. Memory can be allocated in pages, and non-contiguously as well. Most often this is done by calling mmap with a NULL address and a flag to specify that no file descriptor is provided.

In truth, I'm not sure how sbrk interacts with mmap, and if the kernel does enough book keeping to ensure that you don't sbrk on a previously mmap'd region. Thus the lesson is that unless we want to go really deep into some black magic, for now we have to warn the user that they cannot use separate malloc functions when they use `GC_malloc`.

Even if we could scan the heap contiguously we wouldn't really want to. Memory isn't often reclaimed by the kernel in a way that would cause reading freed memory to cause a page fault, and even if it were we wouldn't be able to efficiently determine if a block was mapped in the page table from its address alone. We'd be putting a lot of wasted unused memory back in the cache to mark items that are only referenced to by junk pointers.

Although that is somewhat sad it makes our heap scanning code much simpler and faster. We already know which objects in our heap we want to scan: the ones in the usedp list. 

```c
/*
 * Scan the marked blocks for references to other unmarked blocks.
 */
static void
scan_heap(void)
{
    uintptr_t *vp;
    header_t *bp, *up;

    for (bp = UNTAG(usedp->next); bp != usedp; bp = UNTAG(bp->next)) {
        if (!((uintptr_t)bp->next & 1))
            continue;
        for (vp = (uintptr_t *)(bp + 1);
             vp < (bp + bp->size + 1);
             vp++) {
            uintptr_t v = *vp;
            up = UNTAG(bp->next);
            do {
                if (up != bp &&
                    up + 1 <= v &&
                    up + 1 + up->size > v) {
                    up->next = ((uintptr_t) up->next) | 1;
                    break;
                }
            } while ((up = UNTAG(up->next)) != bp);
        }
    }
}
```

### Scanning the contiguous regions 

Unlike the heap, the BSS and initialized data segments and the stack are all contiguous regions of memory that could possibly contain addresses in our heap. Because each is contiguous, in order to scan them we need to know for each the smallest valid and largest valid memory addresses. 

#### Finding the data segments

Before we discuss how to find the location of the data segments, let's review the order of the segments in memory:

| Address      | Segment                             |
|--------------|-------------------------------------|
| Low address  | Text segment                        |
| ⋮            | Initialized data                    |
|              | BSS                                 |
|              | Heap (grows low to high)            |
|              | ⋮                                   |
| ⋮            | Stack (grows high to low (on i386)) |
| High address | ⋮                                   |

Most modern Unix linkers export two symbols accessible to user programs that are of particular interest to us:

* etext: the address of etext is the last address past the text segment. The initialized data segment immediately follows the text segment and thus the address of etext is the start of the initialized data segment.
* end: the address of end is the start of the heap, or the last address past the end of the BSS.

Since there is no segment between the BSS and initialized segments, we don't have to treat them as separate entities and can scan them by iterating from &etext to &end. 

#### Finding the bottom of the call stack

The stack is a little trickier. The top of the stack is super simple to find using a little bit of inline assembly, as it is stored in the %esp register. However, we'll be using the %ebp register as it ignores a few local variables.

Finding the very bottom of the stack (where the stack began) involves some trickery. Kernels tend to randomize the starting point of the stack for security reasons, so we can't hard code an address. To be honest, I'm not an expert on finding the bottom of the stack, but I have a few rather poor ideas on how you can make an accurate. One possible way is you could scan the call stack for the env pointer, which would be passed as an argument to main. Another way would be to start at the top of the stack and read every subsequent address greater and handling the inexorable SIGSEGV. But we're not going to do it either way. Instead, we're going to exploit the fact that Linux puts the bottom of the stack in a string in a file in the process's entry in the proc directory (phew!). This sounds silly and terribly indirect. Fortunately, I don't feel ridiculous for doing doing it because it's literally _the exact same thing Boehm GC does to find the bottom of the stack!_

### Putting it all together

Now we can make ourselves a little init function. In it, we open the proc file on ourselves and find the bottom of the stack. This is the 28th value printed so we discard the first 27. Boehm GC differs from us in that they only use sys calls to do the file reading in order to avoid the stdlib from using the heap, but we don't really care. 

```c
/*
 * Find the absolute bottom of the stack and set stuff up.
 */
void
GC_init(void)
{
    static int initted;
    FILE *statfp;

    if (initted)
        return;

    initted = 1;

    statfp = fopen("/proc/self/stat", "r");
    assert(statfp != NULL);
    fscanf(statfp,
           "%*d %*s %*c %*d %*d %*d %*d %*d %*u "
           "%*lu %*lu %*lu %*lu %*lu %*lu %*ld %*ld "
           "%*ld %*ld %*ld %*ld %*llu %*lu %*ld "
           "%*lu %*lu %*lu %lu", &stack_bottom);
    fclose(statfp);

    usedp = NULL;
    base.next = freep = &base;
    base.size = 0;
}
```

Now we know the location of every memory region we would need to scan, and thus, we can finally write our explicitly-called collection function:

```c
/*
 * Mark blocks of memory in use and free the ones not in use.
 */
void
GC_collect(void)
{
    header_t *p, *prevp, *tp;
    uintptr_t stack_top;
    extern char end, etext; /* Provided by the linker. */

    if (usedp == NULL)
        return;

    /* Scan the BSS and initialized data segments. */
    scan_region(&etext, &end);

    /* Scan the stack. */
    asm volatile ("movl %%ebp, %0" : "=r" (stack_top));
    scan_region(stack_top, stack_bottom);

    /* Mark from the heap. */
    scan_heap();

    /* And now we collect! */
    for (prevp = usedp, p = UNTAG(usedp->next);; prevp = p, p = UNTAG(p->next)) {
    next_chunk:
        if (!((unsigned int)p->next & 1)) {
            /*
             * The chunk hasn't been marked. Thus, it must be set free. 
             */
            tp = p;
            p = UNTAG(p->next);
            add_to_free_list(tp);

            if (usedp == tp) { 
                usedp = NULL;
                break;
            }

            prevp->next = (uintptr_t)p | ((uintptr_t) prevp->next & 1);
            goto next_chunk;
        }
        p->next = ((uintptr_t) p->next) & ~1;
        if (p == usedp)
            break;
    }
}
```

And that, my friends, is all there is. One simple garbage collector written in C and for C. This code isn't complete in itself, it needs a few little tweaks here in there to get it working, but most of the code mostly holds up on its own. 

## Caveats 

As I said before this code isn't perfect, and almost immediately after this article was published I was given some criticism on certain aspects of it. Here are some reasons why writing a production garbage collector is significantly more complicated than this lesson: 

### This garbage collector is conservative 

Garbage collectors can be classified as either conservative or precise. Conservative garbage collectors are not guaranteed to collect all garbage memory, while precise garbage collectors do provide that promise.

Unfortunately in C it is not possible to determine which memory locations are used as pointers and which are used solely as integers or floating point values. In general, languages do not provide nearly enough reflection to be able to determine things such as what the types of variables allocated on the stack are. And even if C did provide such information it wouldn't help because it allows addresses to be interpreted as numeric values and vice versa. Boehm GC suffers the same problem. If a lot of values on the stack or heap contain values that fall within our heap, which is not uncommon, our code will retain more and more memory.

Thus, garbage collectors are almost always built into the run time of a language that supports them. This allows the collector to be more precise by possibly having access to special run time information. For example, Go's garbage collection achieves precision by pushing relevant type information onto the stack and disallowing conversion between pointers and numeric types.

### This code is not concurrent 

There are a few different ways that garbage collectors can support concurrency:

#### Reentrancy 

A function is said to be reentrant if you can call it before a previous call to the function has been completed. The standard C memory allocation routine implementations are serial algorithms but they are allowed to be called in parallel. Hardening such functions for parallel programming is beyond the scope of this article, so unfortunately not many of the functions written are reentrant. This is because sbrk is fundamentally not reentrant. A better alternative would be to use anonymous page mappings through mmap, which have the added benefit of guaranteed alignment. This doesn't completely solve the problem as we're still modifying static variables. All in all, the problem of making our GC functions reentrant is extremely similar to the problems faced in making typical standard library allocation routines reentrant. That is to say, there is a lot of source material. 

#### Stop the world vs. Parallel collection 

Beyond simply being reentrant a garbage collector can improve execution with parallelism. One problem with our code is that a thread will have to pause its execution whenever it wants to perform a collection. This is known as "stopping the world". To lessen the impact or distribute the cost of these pauses it is possible to write collection and scanning algorithms that run in separate threads from user code. This is an extremely active area of Computer Science research, so I encourage you to explore and do your own research because I definitely don't care enough to. 

### This code does not check registers

In addition to memory regions, registers may very well point to allocated memory. However, in register-poor architectures such as i386 or x64 this is pretty unlikely, and it is more likely that by the time the body of a scan functions is reached any registers storing memory addresses will be pushed to the stack. To avoid any doubt without much overhead it suffices to simply push and caller-saved variables to the stack before scanning the stack. To make it super simple call PUSHA and push all of the registers on the stack before scanning.

## Conclusion

From elementary school through half of high school, I played drums. Every Wednesday at around 4:30 pm I had a drum lesson from a teacher who was quite good.

Whenever I was having trouble learning a new groove or beat or whatever, he would always give me the same diagnosis: I was trying to do everything at once. I looked at the sheet of music, and I simply tried to play with all my hands. But I couldn't. And the reason why is because I didn't know how to play the groove yet, and simply trying to play the groove wasn't how I was going to learn.

So my teacher would enlighten me as to how I could learn: don't try playing everything at once. Learn to play the high-hat part with your right hand. Once you've got that down, learn to play the snare with your left. Do the same with the bass, the tom-toms, and whatever other parts there are. When you have all the individual parts down, slowly begin to add them together. Add them together in pairs, then in threes, and eventually you'll be able to play the entire thing

I never got good at drums, but I did take these lessons to heart in my programming. It's really hard to just start to type out an entire program. The only algorithm you need to write code is divide and conquer. Write the function to allocate memory. Then, write the function to look through memory. Then, write the function that cleans up memory. Finally, add them all together.

As soon as you get past this barrier as a programmer, nothing practical becomes "hard". You may not understand an algorithm, but anyone can understand an algorithm with enough time, paper, and the right book. If a project seems daunting, break it up into its individual parts. You may not know how to write an interpreter, but you sure as hell can write a parser. Find out what else you need to add, and do it.
