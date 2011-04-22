library AudioInterception;
{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,Controls,Windows,Classes,Dialogs,madCodeHook,MMSystem,DateUtils, DirectSound;

{$R *.res}
var
  lastFmt: tWAVEFORMATEX ;//最晚记录格式
  lastlpData: PAnsiChar=nil;      //最晚记录数据指针
  lastdwBufferLength: DWORD;      //最晚记录数据长度
  IPCnameCMD, IPCnameData: string;               //通道名称

  ctx: DWORD = 0;
  voice: Boolean = True;    //enable voice

//管道 传递数据
//管道 传递数据
procedure SendInfo(name : string; pbuf: Pointer; len: DWORD); overload;
var
  mem : TMemoryStream;
begin
  mem := TMemoryStream.Create;
  mem.Write(ctx, SizeOf(DWORD));
  mem.Write(pbuf^, len);

  SendIpcMessage(PAnsiChar(name),mem.Memory, mem.Size, nil, 0, INFINITE, False);
  mem.Free;
end;
procedure SendInfo(name : string; pbuf1: Pointer; len1: DWORD; pbuf2: Pointer; len2: DWORD); overload;
var
  mem : TMemoryStream;
begin
  mem := TMemoryStream.Create;
  mem.Write(ctx, SizeOf(DWORD));
  mem.Write(pbuf1^, len1);
  mem.Write(pbuf2^, len2);

  SendIpcMessage(PAnsiChar(name),mem.Memory, mem.Size);
  mem.Free;
end;

//waveOutWrite
var waveOutWriteNext : function ( hWaveOut: HWAVEOUT;
                                  lpWaveOutHdr: PWaveHdr;
                                  uSize: UINT): MMRESULT; stdcall;

function waveOutWriteCallback(hWaveOut: HWAVEOUT;
                              lpWaveOutHdr: PWaveHdr;
                              uSize: UINT): MMRESULT; stdcall;
begin
  //判断新数据
  if (lastlpData <> lpWaveOutHdr.lpData) or (lastdwBufferLength <> lpWaveOutHdr.dwBufferLength) then
  begin
    lastlpData := lpWaveOutHdr.lpData;
    lastdwBufferLength := lpWaveOutHdr.dwBufferLength;
    //向主进程传递数据
    SendInfo(IPCnameData, lastlpData, lastdwBufferLength);
  end;
  if not voice then
  FillMemory(lastlpData, lastdwBufferLength, 0);
  result := waveOutWriteNext(hWaveOut, lpWaveOutHdr, uSize);
end;

//waveOutOpen
var waveOutOpenNext : function (lphWaveOut: PHWaveOut;
                                uDeviceID: UINT;
                                lpFormat: PWaveFormatEx;
                                dwCallback: DWORD;
                                dwInstance: DWORD;
                                dwFlags: DWORD): MMRESULT; stdcall;
function waveOutOpenCallback(lphWaveOut: PHWaveOut;
                                uDeviceID: UINT;
                                lpFormat: PWaveFormatEx;
                                dwCallback: DWORD;
                                dwInstance: DWORD;
                                dwFlags: DWORD): MMRESULT; stdcall;
begin
    //判断 过滤测试数据
    if dwFlags mod 2 <> 1 then
    begin
      //记录格式
      lastFmt := lpFormat^;
    end;

  result := waveOutOpenNext(lphWaveOut,
                            uDeviceID,
                            lpFormat,
                            dwCallback,
                            dwInstance,
                            dwFlags);
end;


////////////////DSOUND
//step1. hook Api : DirectSoundCreate, DirectSoundCreate8
//step2. hook interface function: IDirectSound, IDirectSound8  ->  CreateSoundBuffer
//step3. hook interface function: IDirectSoundBuffer -> Unlook , SetFormat
// ***************************************************************************

function GetInterfaceMethod(const intf; methodIndex: dword) : pointer;
begin
  result := pointer(pointer(dword(pointer(intf)^) + methodIndex * 4)^);
end;

// ***************************************************************************

//Unlock
var
  UnlockNext :function(self: Pointer; pvAudioPtr1: Pointer; dwAudioBytes1: DWORD; pvAudioPtr2: Pointer; dwAudioBytes2: DWORD): HResult; stdcall = nil;

function UnlockCallback(self: Pointer; pvAudioPtr1: Pointer; dwAudioBytes1: DWORD; pvAudioPtr2: Pointer; dwAudioBytes2: DWORD): HResult; stdcall;
begin

  //向主进程传递数据
  if pvAudioPtr2 <> nil then
  begin
    SendInfo(IPCnameData, pvAudioPtr1, dwAudioBytes1, pvAudioPtr2, dwAudioBytes2);
  //voice control
    if not voice then
    begin
      FillMemory(pvAudioPtr1, dwAudioBytes1, 0);
      FillMemory(pvAudioPtr2, dwAudioBytes2, 0);
    end;
  end
  else
  begin
    if pvAudioPtr1 <> nil then
      SendInfo(IPCnameData, pvAudioPtr1, dwAudioBytes1);
  //voice control
    if not voice then
      FillMemory(pvAudioPtr1, dwAudioBytes1, 0);
  end;

  Result:= UnlockNext(self, pvAudioPtr1, dwAudioBytes1, pvAudioPtr2, dwAudioBytes2);
end;
//SetFormat
var
  SetFormatNext: function(self: Pointer; pcfxFormat: PWaveFormatEx): HResult; stdcall = nil;

function SetFormatCallback(self: Pointer; pcfxFormat: PWaveFormatEx): HResult; stdcall;
begin
  Result:= SetFormatNext(self, pcfxFormat);
  if pcfxFormat <> nil then
    lastFmt:= pcfxFormat^;
end;  
//CreateSoundBuffer
var
  CreateSoundBufferNext: function(self: Pointer; const pcDSBufferDesc: TDSBufferDesc; out ppDSBuffer: IDirectSoundBuffer; pUnkOuter: IUnknown): HResult; stdcall = nil;

function CreateSoundBufferCallback(self: Pointer; const pcDSBufferDesc: TDSBufferDesc; out ppDSBuffer: IDirectSoundBuffer; pUnkOuter: IUnknown): HResult; stdcall;
begin
//!!!!!!!!!!!!!!!!!!self 隐含参数
  Result:= CreateSoundBufferNext(self, pcDSBufferDesc, ppDSBuffer, pUnkOuter);

  if @UnlockNext=nil then
  begin
    HookCode(GetInterfaceMethod(ppDSBuffer,19),@UnlockCallback, @UnlockNext);
  end
  else
    RenewHook(@UnlockNext);

  if @SetFormatNext=nil then
  begin
    HookCode(GetInterfaceMethod(ppDSBuffer,14),@SetFormatCallback, @SetFormatNext);
  end
  else
    RenewHook(@SetFormatNext);

  if pcDSBufferDesc.lpwfxFormat <> nil then
    lastFmt:= pcDSBufferDesc.lpwfxFormat^;


end;

//DirectSoundCreate
var
  DirectSoundCreateNext: function(pcGuidDevice: PGUID; out ppDS: IDirectSound;
                              pUnkOuter: IUnknown): HResult; stdcall = nil;
function DirectSoundCreateCallback(pcGuidDevice: PGUID; out ppDS: IDirectSound; pUnkOuter: IUnknown): HResult; stdcall;

begin
  Result:= DirectSoundCreateNext(pcGuidDevice, ppDS, pUnkOuter);


  if @CreateSoundBufferNext = nil then
  begin
    HookCode(GetInterfaceMethod(ppDS,3),@CreateSoundBufferCallback, @CreateSoundBufferNext);
  end
  else
    RenewHook(@CreateSoundBufferNext);
end;

//DirectSoundCreate8
var
  DirectSoundCreate8Next: function(pcGuidDevice: PGUID; out ppDS8: IDirectSound8;
                              pUnkOuter: IUnknown): HResult; stdcall = nil;
function DirectSoundCreate8Callback(pcGuidDevice: PGUID; out ppDS8: IDirectSound8; pUnkOuter: IUnknown): HResult; stdcall;

begin
  Result:= DirectSoundCreate8Next(pcGuidDevice, ppDS8, pUnkOuter);


  if @CreateSoundBufferNext = nil then
  begin
    HookCode(GetInterfaceMethod(ppDS8,3),@CreateSoundBufferCallback, @CreateSoundBufferNext);
  end
  else
    RenewHook(@CreateSoundBufferNext);
end;


//recive ctx,voice or send fmt
//CMD 0 , send fmt
//CMD 1 , recive ctx
//CMD 2 , recive voice
procedure SendInfoback(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;
var
  mem : TMemoryStream;
  cmd: DWORD;
begin
  mem := TMemoryStream.Create;
  mem.Write(messageBuf^, messageLen);
  mem.Position := 0;
  mem.Read(cmd,SizeOf(cmd));

  //ask fmt
  if cmd = 0 then
  begin
    tWAVEFORMATEX(answerBuf^):= lastFmt;
  end
  else if cmd = 1 then //ctx
  begin
    if mem.Read(ctx, SizeOf(ctx)) = SizeOf(ctx) then
      Boolean(answerBuf^):= True
    else
      Boolean(answerBuf^):= False;
  end
  else if cmd = 2 then  //voice
  begin
    if mem.Read(voice, SizeOf(voice)) = SizeOf(voice) then
      Boolean(answerBuf^):= True
    else
      Boolean(answerBuf^):= False;
  end;

end;


begin

  FillMemory(@lastFmt, SizeOf(lastFmt), 0);
  HookAPI('winmm.dll', 'waveOutWrite', @waveOutWriteCallback, @waveOutWriteNext);
  HookAPI('winmm.dll', 'waveOutOpen', @waveOutOpenCallback, @waveOutOpenNext);
  HookAPI('DSOUND.dll', 'DirectSoundCreate8', @DirectSoundCreate8Callback, @DirectSoundCreate8Next);
  HookAPI('DSOUND.dll', 'DirectSoundCreate', @DirectSoundCreateCallback, @DirectSoundCreateNext);

  IPCnameData:= 'processwavecapture_data_'+ IntToStr(GetCurrentProcessId());
  IPCnameCMD:= 'processwavecapture_CMD_'+ IntToStr(GetCurrentProcessId());

  CreateIpcQueue(PAnsiChar(IPCnameCMD), SendInfoback);
end.


