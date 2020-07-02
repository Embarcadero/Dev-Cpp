The COM DLLs for v5.0 are compiled using Delphi XE2 (including extended RTTI) and include zipx support.  Recompiling with Delphi 2009 and without zipx support should roughly halve the size of the 32-bit DLL.

They can be registered for all users (requires admin rights) using:

  regsvr32 Abbrevia.dll

And for the current user using:

  regsvr32 /i:user /n Abbrevia.dll

To uninstall use:

  regsvr32 /u Abbrevia.dll

or 

  regsvr32 /i:user /n /u Abbrevia.dll