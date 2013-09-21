unit PackmanExitCodesU;

interface

type
  TPackmanExitCode = Integer;

const
  PACKMAN_EXITCODE_ERRCODE_UNKNOWN   : TPackmanExitCode = -1;
  PACKMAN_EXITCODE_NO_ERROR          : TPackmanExitCode = 0;
  PACKMAN_EXITCODE_INVALID_FORMAT    : TPackmanExitCode = 1;
  PACKMAN_EXITCODE_INSTALL_CANCELLED : TPackmanExitCode = 2;
  PACKMAN_EXITCODE_NO_PACKAGE_DESCRIPTION : TPackmanExitCode = 3;
  PACKMAN_EXITCODE_VERSION_NOT_SUPPORTED : TPackmanExitCode = 4;
  PACKMAN_EXITCODE_DEPENDACIES_NOT_MET : TPackmanExitCode = 5;
  PACKMAN_EXITCODE_FILE_NOT_FOUND    : TPackmanExitCode = 6;

implementation

end.