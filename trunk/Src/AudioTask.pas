unit AudioTask;

interface
uses
  Windows, SysUtils, Classes, MMSystem, madCodeHook, WaveMaker, AudioCapture ,StrUtils;


var
  AcList: TStringList;


procedure GetAudioDataFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;
procedure GetAudioHeadFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;

function InjectTarge(TargetPid: DWORD): Boolean;
function UninjectTarge(TargetPid: DWORD): Boolean;
/////////////////////////
function AddTask(tPid: Cardinal; path: string; AOwner: TComponent): Boolean ;
function DelTask(tPid: Cardinal): Boolean ;
function ClearTask(): Boolean ;
procedure StartTask(tPid: Cardinal);
procedure PauseTask(tPid: Cardinal);
procedure ContinueTask(tPid: Cardinal);
procedure StopTask(tPid: Cardinal);

    procedure MakeWav(tPid: Cardinal);
    procedure MakeWavAsOne(tPid: Cardinal);
    procedure MakeAudio(tPid: Cardinal;ForceFormat: string; AudioSampleRate: string);
    procedure MakeAudioAsOne(tPid: Cardinal;ForceFormat: string; AudioSampleRate: string);

function GetWavInfo(tPid: Cardinal; index: Integer; var HWaveOut: Integer;
  var WavFormate: string; var DataSize: Integer ;var bMerge: Boolean): Boolean;
function setMerge(tPid: Cardinal; index: Integer; bMerge: Boolean): Boolean;


procedure CreateAudioTasks();
procedure FreeAudioTasks();

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
  ac : TAudioCapture;
  Lindex: Integer;
  tPid: Cardinal;
begin
  mem := TMemoryStream.Create;
  mem.Write(messageBuf^, messageLen);
  mem.Position := 0;
  mem.Read(HWaveOut,SizeOf(Integer));
  mem.Read(messageBuf^, messageLen - SizeOf(Integer));

  tPid:= StrToInt(StringReplace(name,'AudioInterception_data_','',[]));

  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex >= 0 then
  begin
    ac:=TAudioCapture(AcList.Objects[Lindex]);
    ac.OnReadData(messageBuf, messageLen - SizeOf(Integer), IntToStr(HWaveOut));
  end;
  FreeAndNil(mem);
end;

//获取截获的Head    Stdcall
procedure GetAudioHeadFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;
var
  mem : TMemoryStream;
  HWaveOut : Integer;
  ac : TAudioCapture;
  Lindex: Integer;
  tPid: Cardinal;
begin
  mem := TMemoryStream.Create;
  mem.Write(messageBuf^, messageLen);
  mem.Position := 0;
  mem.Read(HWaveOut,SizeOf(Integer));
  mem.Read(messageBuf^, messageLen - SizeOf(Integer));

  tPid:= StrToInt(StringReplace(name,'AudioInterception_head_','',[]));

  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex>= 0  then
  begin
    ac:=TAudioCapture(AcList.Objects[Lindex]);
    ac.OnReadData(messageBuf, messageLen - SizeOf(Integer), IntToStr(HWaveOut));
  end;
  FreeAndNil(mem);
end;

/////////////////////////////////////////////////////////////////////////////

function InjectTarge(TargetPid: DWORD): Boolean;
var
  PHandle : THandle ;
  ipcnameHead, ipcnameData : string;

begin
  //Get ProcessHandle

  PHandle := OpenProcess(PROCESS_ALL_ACCESS,False,TargetPid);

//Inject the target
  if InjectLibraryA(PHandle, 'AudioInterception.dll') then
  begin
    ipcnameHead := 'AudioInterception_head_' + inttostr(TargetPid);
    ipcnameData := 'AudioInterception_data_' + inttostr(TargetPid);
    CreateIpcQueue(PAnsiChar(ipcnameHead), GetAudioHeadFromDLL);//创建IPC
    CreateIpcQueue(PAnsiChar(ipcnameData), GetAudioDataFromDLL);


    Result:= True;
  end
  else
  Result:= False;


end;  

function UninjectTarge(TargetPid: DWORD): Boolean;
var
  PHandle : THandle ;
  ipcnameHead, ipcnameData : string;
begin
  PHandle := OpenProcess(PROCESS_ALL_ACCESS,False,TargetPid);
  //Uninject the target
  if UninjectLibraryA(PHandle, 'AudioInterception.dll') then
  begin
    ipcnameHead := 'AudioInterception_head_' + inttostr(TargetPid);
    ipcnameData := 'AudioInterception_data_' + inttostr(TargetPid);
    DestroyIpcQueue(PAnsiChar(ipcnameHead));
    DestroyIpcQueue(PAnsiChar(ipcnameData));

    Result:= True;
  end
  else
  Result:= False;
end;  

function AddTask(tPid: Cardinal; path: string; AOwner: TComponent): Boolean ;
var
  ac: TAudioCapture;
begin

  ac:= TAudioCapture.Create(tPid, path, AOwner);

  AcList.AddObject(IntToStr(tPid),TObject(ac));
  Result:= InjectTarge(tPid);

end;

function DelTask(tPid: Cardinal): Boolean ;
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  Result:= UninjectTarge(tPid);
  AcList.Delete(Lindex);
  FreeAndNil(ac);
end;

function ClearTask(): Boolean ;
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Result:= True;
  for Lindex:=0 to AcList.Count - 1 do
  begin
    ac:=TAudioCapture(AcList.Objects[Lindex]);
    if not UninjectTarge(StrToInt(AcList.Strings[Lindex])) then Result:= False;
    AcList.Delete(Lindex);
    FreeAndNil(ac);
  end;
end;

procedure StartTask(tPid: Cardinal);
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
//    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  ac.Start;
end;

procedure PauseTask(tPid: Cardinal);
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
//    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  ac.Pause;
end;

procedure ContinueTask(tPid: Cardinal);
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
//    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  ac.Continue;
end;

procedure StopTask(tPid: Cardinal);
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
//    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  ac.Stop;
end;

procedure MakeWav(tPid: Cardinal);
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
//    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  ac.MakeWav;
end;

procedure MakeWavAsOne(tPid: Cardinal);
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
//    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  ac.MakeWavAsOne;
end;

procedure MakeAudio(tPid: Cardinal;ForceFormat: string; AudioSampleRate: string);
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
//    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  ac.MakeAudio(ForceFormat, AudioSampleRate);
end;

procedure MakeAudioAsOne(tPid: Cardinal;ForceFormat: string; AudioSampleRate: string);
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
//    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  ac.MakeAudioAsOne(ForceFormat, AudioSampleRate);
end;

function GetWavInfo(tPid: Cardinal; index: Integer; var HWaveOut: Integer;
  var WavFormate: string; var DataSize: Integer ;var bMerge: Boolean): Boolean;
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);

  Result:= ac.GetWavInfo(index, HWaveOut, WavFormate, DataSize, bMerge);
end;

function setMerge(tPid: Cardinal; index: Integer; bMerge: Boolean): Boolean;
var
  Lindex: Integer;
  ac: TAudioCapture;
begin
  Lindex:= AcList.IndexOf(IntToStr(tPid));
  if Lindex< 0  then
  begin
    Result:= False;
    Exit;
  end;
  ac:=TAudioCapture(AcList.Objects[Lindex]);
  Result:= ac.setMerge( index, bMerge);
end;


procedure CreateAudioTasks();
begin
  FreeAndNil(AcList);
  AcList:= TStringList.Create;
end;

procedure FreeAudioTasks();
begin
  FreeAndNil(AcList);
end;    
end.
