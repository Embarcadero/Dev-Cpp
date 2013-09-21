1) When compressing with upx (Ultimate Packer for Executables) use:

upx -9 --compress-icons=0 devcpp.exe

Otherwise upx will compress all icons and the file associations will point to nonexisting (moved) icons within devcpp.exe