(*
 * process   Wave capture interface
 * Wave Capture base on Api Hook
 * self process
 *)

unit webProcessWaveCapture;

interface

uses
  Windows,
  SysUtils,
  Classes,
  MMSystem,
  Controls, DirectSound;

procedure register_webprocesswavecapture;
function AskFormat(): string;
function SendVoice(voice : Boolean): Boolean;

implementation

uses
  libavcodec,
  AVCodecStubs,
  libavformat,
  AVFormatStubs,
  libavutil,
  libavutil_error,
  libavutil_log,
  AVUtilStubs,

  FFUtils,
  MyUtils,
  madCodeHook ,StrUtils, TlHelp32;

const
  AUDIO_BLOCK_SIZE = 4096;
  EMPTY_BLOCK_SIZE = 1024;

type
  Pwebprocesswave_grab = ^Twebprocesswave_grab;
  Twebprocesswave_grab = record
//    pid: Cardinal;                   // target Pid
    sample_rate: Integer;               // sample rate
    channels: Integer;                  // channels
    sample_format: Integer;             // sample format
    sample_fmt: TSampleFormat;
    codec_id: TCodecID;
    frame_size: Integer;


    // status
    Started: Integer;
    Stoped: Integer;
    Paused: PInteger;
    mutex: THANDLE;

    // statistics
    pktl: PAVPacketList;
    s: PAVFormatContext;

    //timecontrol
    time_packetstart: Int64;      (* last packet start time *)
    time_packetlast: Int64;       (* last packet last time *)
    sample_usPerByte: Double;         (* one byte = sample_usPerByte microsecond*)
  end;

var
  lastFmt: tWAVEFORMATEX ;//最晚记录格式
  lastlpData: Pointer;      //最晚记录数据指针
  lastdwBufferLength: DWORD;      //最晚记录数据长度
  boolVoice: Boolean;          //target audio enable(1) or disable(0), default 1

  Processctx : Pwebprocesswave_grab;


// AddPacket
procedure AddPacket(messageBuf : pointer; messageLen : dword);
type
  PPAVPacketList = ^PAVPacketList;
var
  LBytes: DWORD;
  ppktl: PPAVPacketList;
  pktl_next: PAVPacketList;
begin

  if (DWORD(Processctx) <> 0)  and (Processctx.Started = 1)   then
  begin
        LBytes:=messageLen - SizeOf(Pwebprocesswave_grab);

        WaitForSingleObject(Processctx.mutex, INFINITE);
        try
          // create packet list
          pktl_next := av_mallocz(SizeOf(TAVPacketList));
          if not Assigned(pktl_next) then
          begin
            Processctx.Stoped := 1;
            Exit;
          end;

          // create packet
          if av_new_packet(@pktl_next.pkt, LBytes) < 0 then
          begin
            av_free(pktl_next);
            Processctx.Stoped := 1;
            Exit;
          end;

          // write wave data
//          mem.Read(pktl_next.pkt.data^, LBytes);
            Move(messageBuf^, pktl_next.pkt.data^, LBytes);
          // add packet to list
          ppktl := @Processctx.pktl;
          while Assigned(ppktl^) do
          ppktl := @ppktl^.next;
          ppktl^ := pktl_next;
        finally
          ReleaseMutex(Processctx.mutex);
        end;
  end;
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
    AddPacket(lastlpData, lastdwBufferLength);

  end;
  if not boolVoice then
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
    AddPacket(pvAudioPtr1, dwAudioBytes1);
    AddPacket(pvAudioPtr2, dwAudioBytes2);
  //voice control
    if not boolVoice then
    begin
      FillMemory(pvAudioPtr1, dwAudioBytes1, 0);
      FillMemory(pvAudioPtr2, dwAudioBytes2, 0);
    end;
  end
  else
  begin
    if pvAudioPtr1 <> nil then
      AddPacket(pvAudioPtr1, dwAudioBytes1);
  //voice control
    if not boolVoice then
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


//ask format   cmd = 0
//return as 'r=44100;c=2;f=16;'
function AskFormat(): string;
begin
  Result:= '';
  if lastFmt.nSamplesPerSec <> 0 then
  Result:= 'r='+inttostr(lastFmt.nSamplesPerSec)+';c='+inttostr(lastFmt.nChannels)
            +';f='+inttostr(lastFmt.wBitsPerSample)+';' ;
end;


//send voice  cmd = 2
//voice
function SendVoice(voice : Boolean): Boolean;
begin
  boolVoice:= voice;
  result := True;
end;


// release resource
procedure ReleaseResource(ctx: Pwebprocesswave_grab);
var
  pktl: PAVPacketList;
begin
  ctx.s := nil;
  ctx.Stoped := 1;

  // close mutex and event
  if ctx.mutex <> 0 then
  begin
    CloseHandle(ctx.mutex);
    ctx.mutex := 0;
  end;

  // free pktl
  while Assigned(ctx.pktl) do
  begin
    pktl := ctx.pktl;
    ctx.pktl := pktl.next;
    av_free(pktl);
  end;
end;

// wave grab demuxer read_header API
function wave_grab_read_header(s: PAVFormatContext; ap: PAVFormatParameters): Integer; cdecl;
var
  ctx: Pwebprocesswave_grab;
  st: PAVStream;
  param: string;
  N, V: string;
begin
  ctx := s.priv_data;   Processctx := s.priv_data;
  ctx.s := s;           Processctx.s := s;

  ctx.frame_size := AUDIO_BLOCK_SIZE;

  // default CD quality
  ctx.sample_rate := 44100;
  ctx.channels := 2;
  ctx.sample_format := 16;

  // process wave capture parameters
  // filename format: <option1>=<param1>;<option2>=<param2>;...
  //  TODO:
  //  r=int: sample rate, default 44100 of (8000, 11025, 22050, 44100)
  //  c=int: channels, default 2 of (1, 2)
  //  f=int: sample format, default 16 of (8, 16)
  //  voice=boolean: target audio enable(1) or disable(0), default 1
  //  pause_pointer=int: integer value of pause flag pointer

  // parse parmameters
  param := string(s.filename);
  while param <> '' do
  begin
    V := Fetch(param, ';');
    N := Fetch(V, '=');
    if SameText(N, 'r') then
      // sample rate
      ctx.sample_rate := StrToIntDef(V, 44100)
    else if SameText(N, 'c') then
      // channels
      ctx.channels := StrToIntDef(V, 2)
    else if SameText(N, 'f') then
      // sample format
      ctx.sample_format := StrToIntDef(V, 16)
    else if SameText(N, 'pause_pointer') then
      // pause flag pointer
      ctx.Paused := Pointer(StrToIntDef(V, 0))
    else if SameText(N, 'voice') then
      // pause flag pointer
      boolVoice := StrToBoolDef(V,True);
  end;

  // check sample rate
  if ctx.sample_rate <= 0 then
  begin
    Result := AVERROR_IO;
    Exit;
  end;

  // check channels
  if not ctx.channels in [1, 2] then
  begin
    Result := AVERROR_IO;
    Exit;
  end;

  // check sample format
  case ctx.sample_format of
    8:
      begin
        ctx.sample_fmt := SAMPLE_FMT_U8;
        ctx.codec_id := CODEC_ID_PCM_U8;
      end;
    16:
      begin
        ctx.sample_fmt := SAMPLE_FMT_S16;
        ctx.codec_id := CODEC_ID_PCM_S16LE;
      end;
  else
    Result := AVERROR_IO;
    Exit;
  end;

  // calculate    usPerByte
  ctx.sample_usPerByte:= 1000000 / (ctx.sample_rate * ctx.channels * ctx.sample_format div 8);

  // create mutex
  ctx.mutex := CreateMutex(nil, False, nil);
  if ctx.mutex = 0 then
  begin
    ReleaseResource(ctx);
    Result := AVERROR_IO;
    Exit;
  end;

  // new stream
  st := av_new_stream(s, 0);
  if st = nil then
  begin
    ReleaseResource(ctx);
    Result := AVERROR_NOMEM;
    Exit;
  end;

  (* take real parameters *)
  st.codec.codec_type   := AVMEDIA_TYPE_AUDIO;
  st.codec.codec_id     := ctx.codec_id;
  st.codec.channels     := ctx.channels;
  st.codec.sample_rate  := ctx.sample_rate;
  st.codec.block_align  := ctx.sample_format * ctx.channels div 8;
  st.codec.sample_fmt   := ctx.sample_fmt;
  st.codec.bit_rate     := ctx.sample_rate * ctx.channels * ctx.sample_format;
  st.codec.frame_size   := ctx.frame_size;
  st.codec.bits_per_coded_sample := ctx.sample_format;

  // !!! to avoid av_find_stream_info() to read packets
  // condition 1
//  st.r_frame_rate := ctx.time_base;
//  st.avg_frame_rate := ctx.time_base;
  // condition 2
  s.flags := s.flags or AVFMT_FLAG_NOPARSE;
  // condition 3
  st.first_dts := 0;
  // condition ALL
  s.probesize := 0;

//  av_set_pts_info(st, 64, 1, ctx.sample_rate);
  av_set_pts_info(st, 64, 1, 1000000); (* 64 bits pts in us *)

  Result := 0;
end;

// wave grab demuxer read_packet API
function wave_grab_read_packet(s: PAVFormatContext; pkt: PAVPacket): Integer; cdecl;
var
  ctx: Pwebprocesswave_grab;
  pktl: PAVPacketList;
  emptyPkt: TAVPacket;
  delay: Int64;
begin

  ctx := s.priv_data;

  // TODO: could we miss one wave buffer?
  if ctx.Started = 0 then
  begin
    ctx.time_packetstart:= av_gettime;
    ctx.time_packetlast:= 0;
    ctx.Started := 1;
  end;

  if ctx.Stoped = 1 then
  begin
    Result := AVERROR_EOF;
    Exit;
  end;

  (* wait based on the sample rate *)
  while True do
  begin
    delay := ctx.time_packetstart + ctx.time_packetlast - av_gettime;
    if delay <= 0 then
    begin
     //if delay <<<0  need do something ?
      Break;
    end;
    Sleep(delay div 1000);
  end;

  //add wave data
    WaitForSingleObject(ctx.mutex, INFINITE);
    pktl := ctx.pktl;
    //data packet
    if Assigned(pktl) then
    begin
      pkt^ := pktl.pkt;
      ctx.pktl := pktl.next;
      av_free(pktl);

      Inc(ctx.time_packetstart, ctx.time_packetlast);
      ctx.time_packetlast:= Round(pkt.size * ctx.sample_usPerByte);
    end
    //empty packet
    else 
    begin
      // create packet
        if av_new_packet(@emptyPkt, EMPTY_BLOCK_SIZE) < 0 then
        begin
          ctx.Stoped := 1;
          Result:= AVERROR_NOMEM;
          Exit;
        end;
      // write wave data
        FillMemory(emptyPkt.data, EMPTY_BLOCK_SIZE, 0);

      // add packet
        pkt^ := emptyPkt;

        Inc(ctx.time_packetstart, ctx.time_packetlast);
        ctx.time_packetlast:= Round(pkt.size * ctx.sample_usPerByte);
    end ;
  ReleaseMutex(ctx.mutex);
    if ctx.Paused^ <> 0 then
    begin
      Result:= AVERROR_EAGAIN;
      Exit;
    end;
  Result := pkt.size;
end;

// wave grab demuxer read_close API
function wave_grab_read_close(s: PAVFormatContext): Integer; cdecl;
var
  ctx: Pwebprocesswave_grab;
begin
  ctx := s.priv_data;
  ReleaseResource(ctx);
  Result := 0;
end;

var
  process_wave_capture_demuxer: TAVInputFormat = (
    name: 'webprocesswavecapture';
    long_name: 'web Process Wave capture using API hook';
    priv_data_size: SizeOf(Twebprocesswave_grab);
    read_header: wave_grab_read_header;
    read_packet: wave_grab_read_packet;
    read_close: wave_grab_read_close;
    flags: AVFMT_NOFILE;
  );

procedure register_webprocesswavecapture;
begin
  RegisterInputFormat(@process_wave_capture_demuxer);

  FillMemory(@lastFmt, SizeOf(lastFmt), 0);
  lastlpData := nil;
  lastdwBufferLength:= 0;
  boolVoice:= True;          //target audio enable(1) or disable(0), default 1
  DWORD(Processctx) := 0;

  HookAPI('winmm.dll', 'waveOutWrite', @waveOutWriteCallback, @waveOutWriteNext);
  HookAPI('winmm.dll', 'waveOutOpen', @waveOutOpenCallback, @waveOutOpenNext);
  HookAPI('DSOUND.dll', 'DirectSoundCreate8', @DirectSoundCreate8Callback, @DirectSoundCreate8Next);
  HookAPI('DSOUND.dll', 'DirectSoundCreate', @DirectSoundCreateCallback, @DirectSoundCreateNext);
end;

end.
