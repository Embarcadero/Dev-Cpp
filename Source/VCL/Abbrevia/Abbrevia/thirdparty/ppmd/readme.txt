This is a C conversion of Dag Ågren's Objective-C conversion of Dmitry Shkarin's original C++ PPMd variant I.1 release.

The C++ code is available from Dmitry's website at http://www.compression.ru/ds/
The Objective-C code is available from The Unarchiver's Google Code page at http://code.google.com/p/theunarchiver/

Retrieved Jan 2011.  Converted to plain C, added InStream to CarrylessRangeCoder.h and removed CSInputBuffer dependency.  Added CreatePPMdModelVariantI/FreePPMdModelVariantI routines to simplify Pascal usage & header translation.

Compile with:
  Release: bcc32 -q -c -u- *.c
  Debug:   bcc32 -q -c -u- -v -y *.c


Dag's original posts in the Info-ZIP forums: http://www.info-zip.org/phpBB3/viewtopic.php?f=4&t=113 ("PPMd support?", Oct 4, 2008).

=============

PPMd is a complete and utter mess. It is somewhat portable, but it is entirely unreadable and uncommented, and relies heavily on C++ and global variables.

I have been spending quite a bit of effort to port the PPMd code to readable and portable C code for The Unarchiver. Currently I have variants G and H working, and am working on I which is what Zip needs. The current code is available here: http://code.google.com/p/theunarchiver/source/browse/#svn/trunk/XADMaster (the PPMd* files). It is currently written mostly in C with a few Objective-C idioms. It should be easy to port it to pure C for use in Info-Zip.

The code is available under the same license as PPMd, which I believe is public domain. Feel free to use it for any purpose, and if you want to use it in Info-Zip, let me know and I will report back when variant I is finished.

=============

Update: I've got variant I working, and I've used it to unpack files from Zip archives. It might have some bugs still, but it largely works.

One issue is that it probably doesn't work on 64-bit architectures, due to the code being very sensitive to the sizes of its structures, and 8-byte pointers would break it. I've partially fixed it to use 32-bit offsets instead of 64-bit pointers, but it's not entirely done yet for variant I.

The original PPMd code doesn't work on 64-bit at all, except for variant J.

==============

The latest version should work fine on 64 bit now. I have no tested it, though.

The pointer size issues arise because PPMd is a quite peculiar algorithm - it uses its own memory allocator, and the exact behaviour of this allocator affects the working of the compression algorithm (specifically, the exact point when the allocator runs out of memory must be the same on compressor and decompressor). The default implementation assumes that all pointers are 4 bytes in size, and thus will not work on 64-bit architectures if normal pointers are used.

My implementation works around this by storing all pointers affected by this as 32-bit offsets instead, regardless of processor word length.

> Does that cause problems with large files, or is it all internal to the compress/expand stuff?

It's all just internal state. PPMd uses a massive internal state to predict and output bytes, one by one.

===============

Here's the basic structure to use:

PPMdSubAllocatorVariantI *alloc=CreateSubAllocatorVariantI(suballocsize);

PPMdModelVariantI model;
StartPPMdModelVariantI(&model,input,alloc,maxorder,modelrestorationmethod);

for(;;)
{
    int byte=NextPPMdVariantIByte(&model);
    if(byte<0) break;
    fputc(byte,stdout);
}

FreeSubAllocatorVariantI(alloc);

There are a couple things to note. "suballocsize", "maxorder" and "modelrestoration" can be found before the start of the data stream - see the specs for more info on that. Note that "suballocsize" is given in bytes, but is stored as the number of megabytes in the file, so multiply it before passing it.

"input" is the biggest problem - my code uses a custom buffered input class, that uses Objective-C calls, so you will need to change that to use something appropriate for Info-Zip. All reading of input happens in CarrylessRangeCoder.m (specifically in the normalization function). It just reads bytes one at a time, so it should be a piece of cake to rewrite it to suit your needs.

The files you'll need should be PPMdContext.*, PPMdVariantI.*, PPMdSubAllocator.*, CarrylessRangeCoder.*. Any files named .m can usually just be renamed .c with minor changes (change #import to #include, and add #ifndef to the .h files).

If you want to get fancy, you can probably combine some of the files and functions since you only need support for one variant instead of several, and throw out some unused code.

==============

It's also worth noting that my code only does PPMd decompression. However, decompression and compression largely use the same functions, and the compression functions are very similar to the decompression functions. If you compare the original PPMdI source code with mine, you can probably figure out how to implement compression if you want it.

(Although you will get a headache from trying to read the original PPMd code.)
