unit AudioCapture;

interface
uses
  Windows, SysUtils, Classes, MMSystem, WaveMaker;
type
  TAudioCapture = class(TObject)
  private
    MakerList : TStringList;
    //TargetPHandle : THandle; //target½ø³Ì¾ä±ú
    TargetPid: DWORD;
    OutPutPath: string;
    DefaultFormate: tWAVEFORMATEX;

    FlagListUsing : Boolean; //Lock MakerList
  public
    constructor Create(Pid: DWORD;Path : string);
    {List Tools}
    procedure ListAdd(name: string ; Maker: TObject);
    procedure ListDel(name: string );
    function ListFind(name: string; var Maker: TObject): Boolean;

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
    procedure MakeAudioFile;
    procedure DeleteTemp();

    procedure SetDefaultFormate(nSamplesPerSec: DWORD; nChannels: Word; wBitsPerSample: Word);





  end;
implementation

{ TAudioCapture }

constructor TAudioCapture.Create(Pid: DWORD; Path: string);
begin
  //inherited;
  TargetPid:= Pid;
  OutPutPath:= Path;
  MakerList:= TStringList.Create;
  FlagListUsing:= False;
  SetDefaultFormate(44100,2,16);
end;

procedure TAudioCapture.ListAdd(name: string; Maker: TObject);
begin

  MakerList.AddObject(name,Maker);
end;

procedure TAudioCapture.ListDel(name: string);
var
  ListIndex: Integer;
begin
  if MakerList.Find(name, ListIndex) then
  begin

    MakerList.Delete(ListIndex);
  end;
  
end;

function TAudioCapture.ListFind(name: string; var Maker: TObject): Boolean;
var
  ListIndex: Integer;
begin

  if MakerList.Find(name, ListIndex) then
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
//  while FlagListUsing do ;
//  FlagListUsing:= True;

  if ListFind(name,wav) then
  begin
    if TWaveMaker(wav).IsRecording then TWaveMaker(wav).WriteTempData(Buf,Len);
  end
  else
  begin
    TWaveMaker(wav):= TWaveMaker.Create(OutPutPath,TargetPid,StrToInt(name));
    ListAdd(name,wav);
  end;

//  FlagListUsing:= False;
end;

procedure TAudioCapture.OnReadFormate(Buf: Pointer; name: string);
var
  wav: TObject;
begin
//  while FlagListUsing do ;
//  FlagListUsing:= True;

  if ListFind(name,wav) then
  begin
    TWaveMaker(wav).SetWavFormate(PWaveFormatEx(Buf)^);
  end
  else
  begin
    TWaveMaker(wav):= TWaveMaker.Create(OutPutPath,TargetPid,StrToInt(name));
    ListAdd(name,wav);
    TWaveMaker(wav).SetWavFormate(PWaveFormatEx(Buf)^);
  end;
  
//  FlagListUsing:= False;
end;

procedure TAudioCapture.Start;
var
  i: Integer;
begin
  for i:= 0 to MakerList.Count-1 do
  begin
    TWaveMaker(MakerList.Objects[i]).Start;
  end;

end;

procedure TAudioCapture.Pause;
var
  i: Integer;
begin
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
    TWaveMaker(MakerList.Objects[i]).Continue;
  end;

end;

procedure TAudioCapture.Stop;
var
  i: Integer;
begin
  for i:= 0 to MakerList.Count-1 do
  begin
    TWaveMaker(MakerList.Objects[i]).Stop;
  end;

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

procedure TAudioCapture.MakeAudioFile;
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

end.
