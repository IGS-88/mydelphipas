(*
 * process   Wave capture interface
 * Wave Capture base on Api Hook
 *)

unit ProcessWaveCapture;

interface

uses
  Windows,
  SysUtils,
  Classes,
  MMSystem;

procedure register_processwavecapture;
function InjectTarget(TargetPid: DWORD): Boolean;
function AskFormat(TargetPid: DWORD): string;
function SendVoice(TargetPid: DWORD; voice : Boolean): Boolean;

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
  madCodeHook ,StrUtils;

const
  AUDIO_BLOCK_SIZE = 4096;
  EMPTY_BLOCK_SIZE = 1024;

type
  Pprocesswave_grab = ^Tprocesswave_grab;
  Tprocesswave_grab = record
    pid: Cardinal;                   // target Pid
    sample_rate: Integer;               // sample rate
    channels: Integer;                  // channels
    sample_format: Integer;             // sample format
    sample_fmt: TSampleFormat;
    codec_id: TCodecID;
    frame_size: Integer;
    voice: Boolean;          //target audio enable(1) or disable(0), default 1

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

// wave Data from dll   Stdcall
procedure GetAudioDataFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;
type
  PPAVPacketList = ^PAVPacketList;
var
//  mem : TMemoryStream;
  ctx: Pprocesswave_grab;
  LBytes: DWORD;
  ppktl: PPAVPacketList;
  pktl_next: PAVPacketList;
begin
//  mem := TMemoryStream.Create;
//  mem.Write(messageBuf^, messageLen);
//  mem.Position := 0;
//  mem.Read(ctx,SizeOf(Pprocesswave_grab));

//CopyMemory(@ctx, messageBuf, SizeOf(Pprocesswave_grab));
  Move( messageBuf^, ctx,SizeOf(Pprocesswave_grab));
  Inc(DWORD(messageBuf),SizeOf(Pprocesswave_grab));

  if (DWORD(ctx) <> 0) and (ctx.Started = 1) then
  begin
        LBytes:=messageLen - SizeOf(Pprocesswave_grab);

        WaitForSingleObject(ctx.mutex, INFINITE);
        try
          // create packet list
          pktl_next := av_mallocz(SizeOf(TAVPacketList));
          if not Assigned(pktl_next) then
          begin
            ctx.Stoped := 1;
            Exit;
          end;

          // create packet
          if av_new_packet(@pktl_next.pkt, LBytes) < 0 then
          begin
            av_free(pktl_next);
            ctx.Stoped := 1;
            Exit;
          end;

          // write wave data
//          mem.Read(pktl_next.pkt.data^, LBytes);
            Move(messageBuf^, pktl_next.pkt.data^, LBytes);
          // add packet to list
          ppktl := @ctx.pktl;
          while Assigned(ppktl^) do
          ppktl := @ppktl^.next;
          ppktl^ := pktl_next;
        finally
          ReleaseMutex(ctx.mutex);
        end;
  end;
//  FreeAndNil(mem);
end;

//ask format   cmd = 0
//return as 'r=44100;c=2;f=16;'
function AskFormat(TargetPid: DWORD): string;
var
  CMD: DWORD;
  ipcnameCMD : string;
  fmt: tWAVEFORMATEX;
begin
  Result:= '';
  CMD:= 0;
  ipcnameCMD:= 'processwavecapture_CMD_' + inttostr(TargetPid);
  SendIpcMessage(PAnsiChar(ipcnameCMD), @CMD, SizeOf(CMD),
                  @fmt, sizeOf(fmt));
  if fmt.nSamplesPerSec <> 0 then
  Result:= 'r='+inttostr(fmt.nSamplesPerSec)+';c='+inttostr(fmt.nChannels)
            +';f='+inttostr(fmt.wBitsPerSample)+';' ;
end;

//send ctx   cmd = 1
//ctx : a pointer of Tprocesswave_grab
function SendCTX(TargetPid: DWORD; ctx: Pprocesswave_grab): Boolean;
var
  CMD: DWORD;
  ipcnameCMD : string;
  mem : TMemoryStream;
begin
  CMD:= 1;
  mem:= TMemoryStream.Create;
  mem.Write(CMD, SizeOf(CMD));
  mem.Write(ctx, SizeOf(ctx));

  ipcnameCMD:= 'processwavecapture_CMD_' + inttostr(TargetPid);
  result := false;
  SendIpcMessage(PAnsiChar(ipcnameCMD), mem.Memory, mem.Size,
                  @result, sizeOf(result));
  mem.Free;
end;

//send voice  cmd = 2
//voice
function SendVoice(TargetPid: DWORD; voice : Boolean): Boolean;
var
  ipcnameCMD : string;
  CMD: DWORD;
  mem : TMemoryStream;
begin
  CMD:= 2;
  mem:= TMemoryStream.Create;
  mem.Write(CMD, SizeOf(CMD));
  mem.Write(voice, SizeOf(voice));

  ipcnameCMD:= 'processwavecapture_CMD_' + inttostr(TargetPid);
  result := false;
  SendIpcMessage(PAnsiChar(ipcnameCMD), mem.Memory, mem.Size,
                  @result, sizeOf(result));
end;

//inject target
function InjectTarget(TargetPid: DWORD): Boolean;
var
  PHandle : THandle ;
  ipcnameData : string;
begin
  //Get ProcessHandle
  PHandle := OpenProcess(PROCESS_ALL_ACCESS,False,TargetPid);
//Inject the target
  if InjectLibraryA(PHandle, 'AudioInterception.dll') then
  begin
    ipcnameData := 'processwavecapture_data_' + inttostr(TargetPid);
    //����IPC
    if CreateIpcQueue(PAnsiChar(ipcnameData), GetAudioDataFromDLL)
    then Result:= True
    else Result:= False;
  end
  else
  Result:= False;
end;
//Uninject target
function UninjectTarget(TargetPid: DWORD): Boolean;
var
  PHandle : THandle ;
  ipcnameData : string;
begin
  PHandle := OpenProcess(PROCESS_ALL_ACCESS,False,TargetPid);
  //Uninject the target
  if UninjectLibraryA(PHandle, 'AudioInterception.dll') then
  begin
    ipcnameData := 'processwavecapture_data_' + inttostr(TargetPid);
    if DestroyIpcQueue(PAnsiChar(ipcnameData))
    then Result:= True
    else Result:= False;
  end
  else
  Result:= False;
end;

// release resource
procedure ReleaseResource(ctx: Pprocesswave_grab);
var
  pktl: PAVPacketList;
begin
  ctx.s := nil;
  ctx.Stoped := 1;

  // Uninject target
  if ctx.pid <> 0 then
  begin
    UninjectTarget(ctx.pid);
    ctx.pid := 0;
  end;

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
  ctx: Pprocesswave_grab;
  st: PAVStream;
  param: string;
  N, V: string;
begin
  ctx := s.priv_data;        
  ctx.s := s;

  ctx.frame_size := AUDIO_BLOCK_SIZE;

  ctx.pid := 0;

  ctx.voice:= True;
  // default CD quality
  ctx.sample_rate := 44100;
  ctx.channels := 2;
  ctx.sample_format := 16;

  // process wave capture parameters
  // filename format: <option1>=<param1>;<option2>=<param2>;...
  //  TODO: pid=int: pid of target process, default 0
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
    if SameText(N, 'pid') then
    begin
      // index of sound card
      ctx.pid:= StrToIntDef(V, 0)
    end
    else if SameText(N, 'r') then
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
      ctx.voice := StrToBoolDef(V,True);
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

////  inject the target
//  if not InjectTarget(ctx.pid) then
//  begin
//    ReleaseResource(ctx);
//    Result := AVERROR_IO;
//    Exit;
//  end;

  // send ctx
  if not SendCTX(ctx.pid, ctx) then
  begin
    ReleaseResource(ctx);
    Result := AVERROR_IO;
    Exit;
  end;

  // send voice
  if not SendVoice(ctx.pid, ctx.voice) then
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
  ctx: Pprocesswave_grab;
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
    end;

    ReleaseMutex(ctx.mutex);

  Result := pkt.size;
end;

// wave grab demuxer read_close API
function wave_grab_read_close(s: PAVFormatContext): Integer; cdecl;
var
  ctx: Pprocesswave_grab;
begin
  ctx := s.priv_data;
  ReleaseResource(ctx);
  Result := 0;
end;

var
  process_wave_capture_demuxer: TAVInputFormat = (
    name: 'processwavecapture';
    long_name: 'Process Wave capture using waveIn functions';
    priv_data_size: SizeOf(Tprocesswave_grab);
    read_header: wave_grab_read_header;
    read_packet: wave_grab_read_packet;
    read_close: wave_grab_read_close;
    flags: AVFMT_NOFILE;
  );

procedure register_processwavecapture;
begin
  RegisterInputFormat(@process_wave_capture_demuxer);
end;

end.
