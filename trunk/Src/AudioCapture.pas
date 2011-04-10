unit AudioCapture;

interface
uses
  Windows, SysUtils, Classes, MMSystem, WaveMaker, DateUtils,
  FFBaseComponent, FFEncode, StdCtrls, StreamProtocol, MyUtils, UWating;
type
  TAudioCapture = class(TObject)
  private
    MakerList : TStringList;
    //TargetPHandle : THandle; //target½ø³Ì¾ä±ú
    TargetPid: DWORD;
    OutPutPath: string;
    DefaultFormate: tWAVEFORMATEX;
    StartTime , StopTime : TDateTime;
    StateAllRecording: Boolean;

    FFEncoder1: TFFEncoder;

    fwaiting: TWaiting;


    {List Tools}
    procedure ListAdd(name: string ; Maker: TObject);
    procedure ListDel(name: string );
    function ListFind(name: string; var Maker: TObject): Boolean;
//    procedure MakeAudioFiles;

  public
    StateConverting: Boolean;
    constructor Create(Pid: DWORD;Path : string; AOwner: TComponent);

    {manage data from Dll}
    procedure OnReadData(Buf: Pointer; Len: Cardinal; name: string);
    procedure OnReadFormate(Buf: Pointer; name: string);

    {Audio Tools}
    procedure Start;
   // procedure Start(name: string); overload;
    procedure Pause;
   // procedure Pause(name: string); overload;
    procedure Continue;
   // procedure Continue(name: string); overload;
    procedure Stop;
   // procedure Stop(name: string); overload;

    procedure MakeWav;
    procedure MakeWavAsOne;
    procedure MakeAudio(ForceFormat: string; AudioSampleRate: string);
    procedure MakeAudioAsOne(ForceFormat: string; AudioSampleRate: string);
    procedure ConverteAudio(inputpath: string; outputpath: string; ForceFormat: string; AudioSampleRate: string);
    procedure DeleteTemp();

    procedure SetDefaultFormate(nSamplesPerSec: DWORD; nChannels: Word; wBitsPerSample: Word);

    function GetWavInfo(index: Integer; var HWaveOut: Integer; var WavFormate: string; var DataSize: Integer ;var bMerge: Boolean): Boolean;
    function setMerge(index: Integer; bMerge: Boolean): Boolean;

    procedure FFEncoderTerminate(Sender: TObject; const ATerminateInfo: TTerminateInfo);
  end;
implementation

{ TAudioCapture }

constructor TAudioCapture.Create(Pid: DWORD; Path: string; AOwner: TComponent);
begin
  //inherited;
  TargetPid:= Pid;
  OutPutPath:= Path;
  MakerList:= TStringList.Create;
  StateAllRecording:= False;
  StateConverting:= False;

  SetDefaultFormate(44100,2,16);
  FFEncoder1 := TFFEncoder.Create(AOwner);
  FFEncoder1.OnTerminate:= FFEncoderTerminate;
end;

procedure TAudioCapture.ListAdd(name: string; Maker: TObject);
begin

  MakerList.AddObject(name,Maker);
end;

procedure TAudioCapture.ListDel(name: string);
var
  ListIndex: Integer;
begin
  ListIndex:=  MakerList.IndexOf(name);
  if ListIndex >= 0 then
  begin
    MakerList.Delete(ListIndex);
  end;
end;

function TAudioCapture.ListFind(name: string; var Maker: TObject): Boolean;
var
  ListIndex: Integer;
begin
  ListIndex:=  MakerList.IndexOf(name);
  if ListIndex >= 0 then
  begin
    Maker:= MakerList.Objects[ListIndex];
    Result:=  True;
  end
  else
    Result:= False;
end;

procedure TAudioCapture.OnReadData(Buf: Pointer; Len: Cardinal; name: string);
var
  wav: TObject;
begin

  if ListFind(name,wav) then
  begin
    if TWaveMaker(wav).IsRecording then TWaveMaker(wav).WriteTempData(Buf,Len);
  end
  else
  begin
    TWaveMaker(wav):= TWaveMaker.Create(OutPutPath,TargetPid,StrToInt(name));
    ListAdd(name,wav);
    if StateAllRecording then TWaveMaker(wav).Recording;
  end;

end;

procedure TAudioCapture.OnReadFormate(Buf: Pointer; name: string);
var
  wav: TObject;
begin

  if ListFind(name,wav) then
  begin
    TWaveMaker(wav).SetWavFormate(PWaveFormatEx(Buf)^);
  end
  else
  begin
    TWaveMaker(wav):= TWaveMaker.Create(OutPutPath,TargetPid,StrToInt(name));
    ListAdd(name,wav);
    TWaveMaker(wav).SetWavFormate(PWaveFormatEx(Buf)^);
    if StateAllRecording then TWaveMaker(wav).Recording;
  end;

end;

procedure TAudioCapture.Start;
var
  i: Integer;
begin
  StartTime:= Now;
  for i:= 0 to MakerList.Count-1 do
  begin
    TWaveMaker(MakerList.Objects[i]).Recording;
  end;
    StateAllRecording:= True;

end;

procedure TAudioCapture.Pause;
var
  i: Integer;
begin
  StateAllRecording:= False;
  for i:= 0 to MakerList.Count-1 do
  begin
    TWaveMaker(MakerList.Objects[i]).Pause;
  end;

end;

procedure TAudioCapture.Continue;
var
  i: Integer;
begin
  for i:= 0 to MakerList.Count-1 do
  begin
      TWaveMaker(MakerList.Objects[i]).Recording;
  end;
    StateAllRecording:= True;
end;

procedure TAudioCapture.Stop;
var
  i: Integer;
begin
  StateAllRecording:= False;
  for i:= 0 to MakerList.Count-1 do
  begin
    TWaveMaker(MakerList.Objects[i]).Stop;
  end;
  StopTime:= Now;
end;

procedure TAudioCapture.DeleteTemp;
var
  i: Integer;
begin
  for i:= 0 to MakerList.Count-1 do
  begin
    TWaveMaker(MakerList.Objects[i]).Stop;
    TWaveMaker(MakerList.Objects[i]).DeleteTemp;
  end;
end;


procedure TAudioCapture.SetDefaultFormate(nSamplesPerSec: DWORD; nChannels,
  wBitsPerSample: Word);
begin
  DefaultFormate.wFormatTag := WAVE_FORMAT_PCM;
  DefaultFormate.nChannels := nChannels;
  DefaultFormate.nSamplesPerSec := nSamplesPerSec;
  DefaultFormate.wBitsPerSample := wBitsPerSample;

  DefaultFormate.nBlockAlign := wBitsPerSample*nChannels div 8;
  DefaultFormate.nAvgBytesPerSec := nSamplesPerSec * nChannels * wBitsPerSample div 8;
  DefaultFormate.cbSize := 0;
end;


function TAudioCapture.GetWavInfo(index: Integer; var HWaveOut: Integer;
  var WavFormate: string; var DataSize: Integer ;var bMerge: Boolean): Boolean;
var
  wav : TObject;
  nSamplesPerSec: DWORD;
  nChannels: Word;
  wBitsPerSample: Word;
begin
  if index < MakerList.Count then
  begin
    wav:= MakerList.Objects[index];
    HWaveOut:= TWaveMaker(wav).GetHWaveOut;
    DataSize:= TWaveMaker(wav).GetWaveDataSize div 1024;
    TWaveMaker(wav).GetWavFormate(nSamplesPerSec, nChannels, wBitsPerSample);
    WavFormate:= inttostr(nSamplesPerSec)+'Hz_'+inttostr(nChannels)+'_'+inttostr(wBitsPerSample)+'bit';
//////////////////
    WavFormate:= WavFormate + DateTimeToStr(TWaveMaker(wav).GetStartTime);
    bMerge:= TWaveMaker(wav).IsMerge;
    Result:= True;
  end
  else Result:= False;

end;

function TAudioCapture.setMerge(index: Integer; bMerge: Boolean): Boolean;
var
  wav: TObject;
begin
  if index < MakerList.Count then
  begin
    wav:= MakerList.Objects[index];
    TWaveMaker(wav).setMerge(bMerge);
    Result:= True;
  end
  else Result:= False;
end;

procedure TAudioCapture.MakeWav;
var
  i: Integer;
begin
  for i:= 0 to MakerList.Count-1 do
  begin
    if TWaveMaker(MakerList.Objects[i]).IsFormateReady then
      TWaveMaker(MakerList.Objects[i]).MakeAudioFile
    else
      begin
        TWaveMaker(MakerList.Objects[i]).SetWavFormate(DefaultFormate);
        TWaveMaker(MakerList.Objects[i]).MakeAudioFile;
      end;
//    TWaveMaker(MakerList.Objects[i]).DeleteTemp;
  end;

end;

procedure TAudioCapture.MakeWavAsOne;
type
  TWaveHeader = record
    Riff_ckid      : DWORD;
    Riff_cksize    : DWORD;
    Riff_fccType   : DWORD;
    fmt_ckid       : DWORD;
    fmt_cksize     : DWORD;
    wFormatTag     : Word;
    nChannels      : Word;
    nSamplesPerSec : DWORD;
    nAvgBytesPerSec: DWORD;
    nBlockAlign    : Word;
    wBitsPerSample : Word;
    data_ckid      : DWORD;
    data_cksize    : DWORD;
  end;
var
  wh: TWaveHeader;
  i: Integer;
  wavFile : TFileStream;
  tempfile: TStream;
  SampleSize: Int64; //in byte
  timeINs: Double; //x.xxx  s
  timeINbyte: Int64;
begin

  wavFile := TFileStream.Create(OutPutPath+'\Audio_'+inttostr(TargetPid)+'.wav', fmCreate);
  wavFile.Seek(44,soFromBeginning);
  SampleSize:=DefaultFormate.nChannels * DefaultFormate.wBitsPerSample div 8;

  for i:= 0 to MakerList.Count-1 do
  begin
    if TWaveMaker(MakerList.Objects[i]).IsMerge then
    begin

    timeINs:= MilliSecondsBetween(StartTime, TWaveMaker(MakerList.Objects[i]).GetStartTime)/1000;
    timeINbyte:=Round(timeINs * DefaultFormate.nSamplesPerSec) * SampleSize ;
    wavFile.Seek(timeINbyte-wavFile.Position,soFromCurrent);

    tempfile:= TFileStream.Create(OutPutPath+'\'+inttostr(TargetPid)+'_'+inttostr(TWaveMaker(MakerList.Objects[i]).GetHWaveOut)+'.wavtemp',fmOpenRead);
    tempfile.Seek(44,soFromBeginning);

    wavFile.CopyFrom(tempfile, tempfile.Size-44);
    FreeAndNil(tempfile);

//    TWaveMaker(MakerList.Objects[i]).DeleteTemp;

    end;
  end;

  timeINs:= MilliSecondsBetween(StartTime, StopTime)/1000;
  timeINbyte:=Round(timeINs * DefaultFormate.nSamplesPerSec) * SampleSize ;
  wavFile.Seek(timeINbyte-wavFile.Position -3,soFromCurrent );
  wavFile.Write('ZGM', 3);        //////////////////////////

  wh.Riff_ckid := FOURCC_RIFF;
  wh.Riff_cksize := wavFile.Size - 8;
  wh.Riff_fccType := mmioStringToFOURCC('WAVE', 0);
  wh.fmt_ckid := mmioStringToFOURCC('fmt', 0);
  wh.fmt_cksize := 16;
  wh.wFormatTag := DefaultFormate.wFormatTag;
  wh.nChannels := DefaultFormate.nChannels;
  wh.nSamplesPerSec := DefaultFormate.nSamplesPerSec;
  wh.nAvgBytesPerSec := DefaultFormate.nAvgBytesPerSec;
  wh.nBlockAlign := DefaultFormate.nBlockAlign;
  wh.wBitsPerSample := DefaultFormate.wBitsPerSample;
  wh.data_ckid := mmioStringToFOURCC('data', 0);
  wh.data_cksize := wavFile.Size - 44;

    wavFile.Seek(0,soFromBeginning);
    wavFile.Write(wh, SizeOf(TWaveHeader));

  FreeAndNil(wavFile);

end;

procedure TAudioCapture.ConverteAudio(inputpath: string; outputpath: string; ForceFormat: string; AudioSampleRate: string);
begin

end;

procedure TAudioCapture.MakeAudio(ForceFormat: string; AudioSampleRate: string);
var
  i: Integer;
  iPath, oPath: string;

//  IO: TInputOptions;
  OO: TOutputOptions;
  LIndex: Integer;
begin

  if not FFEncoder1.AVLibLoaded then
  begin
      if not FFEncoder1.LoadAVLib(ExePath + 'LibAV') then
      begin
      Exit;
      end;
       register_stream_protocol;
  end;
    FFEncoder1.ClearTasks;
    InitOutputOptions(@OO);
  // FLV format output options
    OO.ForceFormat := ForceFormat;      {Do not Localize}
  // FLV only supports this three sample rate, (44100, 22050, 11025).
    OO.AudioSampleRate := StrToInt(AudioSampleRate);

  for i:= 0 to MakerList.Count-1 do
  begin
    if TWaveMaker(MakerList.Objects[i]).IsFormateReady then
      TWaveMaker(MakerList.Objects[i]).MakeAudioFile
    else
      begin
        TWaveMaker(MakerList.Objects[i]).SetWavFormate(DefaultFormate);
        TWaveMaker(MakerList.Objects[i]).MakeAudioFile;
      end;


      iPath:= OutPutPath + '\' + IntToStr(TargetPid)+'_'+ IntToStr(TWaveMaker(MakerList.Objects[i]).GetHWaveOut)+'.wav';
      oPath:= OutPutPath + '\' + IntToStr(TargetPid)+'_'+ IntToStr(TWaveMaker(MakerList.Objects[i]).GetHWaveOut)+'.'+ForceFormat;
      if FileExists(iPath) then
      begin
        LIndex:= FFEncoder1.AddTask(iPath);
        FFEncoder1.SetOutputFile(LIndex, oPath, @OO) ;
      end;
  end;
  if FFEncoder1.TasksCount <> 0 then
  begin
    FFEncoder1.Start(1);
    fwaiting:= TWaiting.Create(nil);
    fwaiting.ShowModal;
  end;
end;

procedure TAudioCapture.MakeAudioAsOne(ForceFormat: string; AudioSampleRate: string);
var
  iPath, oPath: string;
  IO: TInputOptions;
  OO: TOutputOptions;
  LIndex: Integer;
begin

  MakeWavAsOne;
  iPath:= OutPutPath + '\Audio_' + IntToStr(TargetPid) +'.wav';
  oPath:= OutPutPath + '\Audio_' + IntToStr(TargetPid) +'.'+ ForceFormat;
  if FileExists(iPath) then
  begin
    if not FFEncoder1.AVLibLoaded then
    begin
      if not FFEncoder1.LoadAVLib(ExePath + 'LibAV') then
      begin
      Exit;
      end;
       register_stream_protocol;
   end;
    FFEncoder1.ClearTasks;

    InitInputOptions(@IO);

    LIndex:= FFEncoder1.AddTask(iPath, @IO);

    InitOutputOptions(@OO);

  // FLV format output options
    OO.ForceFormat := ForceFormat;      {Do not Localize}
  // FLV only supports this three sample rate, (44100, 22050, 11025).
    OO.AudioSampleRate := StrToInt(AudioSampleRate);

    FFEncoder1.SetOutputFile(LIndex, oPath, @OO) ;

    FFEncoder1.Start(1);

    fwaiting:= TWaiting.Create(nil);
    fwaiting.ShowModal;
  end;

end;

procedure TAudioCapture.FFEncoderTerminate(Sender: TObject;
  const ATerminateInfo: TTerminateInfo);
begin
  fwaiting.Close;
end;

end.
