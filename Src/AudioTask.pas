unit AudioTask;

interface
uses
  Windows, SysUtils, Classes, MMSystem, madCodeHook, WaveMaker, AudioCapture ;


var
  Pid : DWORD = 0;//target进程pid

  ipcnameHead, ipcnameData : string;
  Wav : TWaveMaker;

  Ac: TAudioCapture;

procedure GetAudioDataFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;
procedure GetAudioHeadFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;

function InjectTarge(TargetPid: DWORD): Boolean;
function UninjectTarge(TargetPid: DWORD): Boolean;


implementation

//处理Target数据
/////////////////////////////////////////////////////////////////////////////

//获取截获的Data    Stdcall
procedure GetAudioDataFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;
var
  mem : TMemoryStream;
  HWaveOut : Integer;
begin
  mem := TMemoryStream.Create;
  mem.Write(messageBuf^, messageLen);
  mem.Position := 0;
  mem.Read(HWaveOut,SizeOf(Integer));
  mem.Read(messageBuf^, messageLen - SizeOf(Integer));

  //Mylog.Write('D_'+IntToStr(HWaveOut));
  Ac.OnReadData(messageBuf, messageLen - SizeOf(Integer), IntToStr(HWaveOut));

  //if Wav.IsRecording then Wav.WriteTempData(messageBuf,messageLen - SizeOf(Integer));



end;

//获取截获的Head    Stdcall
procedure GetAudioHeadFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;
var
  mem : TMemoryStream;
  HWaveOut : Integer;
begin
  mem := TMemoryStream.Create;
  mem.Write(messageBuf^, messageLen);
  mem.Position := 0;
  mem.Read(HWaveOut,SizeOf(Integer));
  mem.Read(messageBuf^, messageLen - SizeOf(Integer));

  //Mylog.Write('H_'+IntToStr(HWaveOut));
  Ac.OnReadFormate(messageBuf, IntToStr(HWaveOut));
  //Wav.SetWavFormate(PWaveFormatEx(messageBuf)^);

end;

/////////////////////////////////////////////////////////////////////////////

function InjectTarge(TargetPid: DWORD): Boolean;
var
  PHandle : THandle ;
  b: Boolean;
begin
  //Get ProcessHandle
  Pid := TargetPid;
  PHandle := OpenProcess(PROCESS_ALL_ACCESS,False,TargetPid);

//Inject the target
  if InjectLibraryA(PHandle, 'AudioInterception.dll') then
  begin
    ipcnameHead := 'AudioInterception_head_' + inttostr(Pid);
    ipcnameData := 'AudioInterception_data_' + inttostr(Pid);
    b := CreateIpcQueue(PAnsiChar(ipcnameHead), GetAudioHeadFromDLL);//创建IPC
    if b then
    begin
      b := b;
    end;
    b := CreateIpcQueue(PAnsiChar(ipcnameData), GetAudioDataFromDLL);
    if b then
    begin
      b := b;
    end;
    Result:= True;
  end
  else
  Result:= False;


end;  

function UninjectTarge(TargetPid: DWORD): Boolean;
var
  PHandle : THandle ;
begin
  PHandle := OpenProcess(PROCESS_ALL_ACCESS,False,TargetPid);
  //Uninject the target
  if UninjectLibraryA(PHandle, 'AudioInterception.dll') then
  begin
    ipcnameHead := 'AudioInterception_head_' + inttostr(TargetPid);
    ipcnameData := 'AudioInterception_data_' + inttostr(TargetPid);
    DestroyIpcQueue(PAnsiChar(ipcnameHead));
    DestroyIpcQueue(PAnsiChar(ipcnameData));

    Pid := 0;
    Result:= True;
  end
  else
  Result:= False;
end;  





end.
