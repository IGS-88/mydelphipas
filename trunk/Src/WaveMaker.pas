unit WaveMaker;

interface

uses
  Windows, SysUtils,Classes,MMSystem;

type
  TWaveMaker = class(TObject)
  private
    TargetPid: DWORD;               //目标Pid
    TargetHWaveOut : Integer;        //目标设备句柄
    OutPutPath: string;                //输出地址  无后缀
    StartTime: TDateTime;              //记录开始写数据的时间

    WavFormate: tWAVEFORMATEX;           //wav格式
    WaveDataSize: Integer;               //wav数据大小

    StateStart: Boolean;                //开始标记 1: 已经开始写入数据 temp文件已生成
    StateRecording: Boolean;            //录制标记  1：可写入数据
    StateFormateReady: Boolean;            //格式标记
    StateWriting: Boolean;              //写标记
    StateMerge: Boolean;
    tempFileData : TFileStream;

  public
    constructor Create(Path :string ; Pid : DWORD; HWaveOut : Integer);

    procedure WriteTempData(Buf: pointer; Len: dword);//写数据
    procedure SetWavFormate(WHead: tWAVEFORMATEX);   //设置格式
    procedure SetWavInfo(nSamplesPerSec: DWORD; nChannels: Word; wBitsPerSample: Word);
    procedure setMerge(b : Boolean); // 设置是否参与合成

    function IsStart(): Boolean;   //验证是否已开始 temp文件已生成
    function IsRecording(): Boolean;   //验证是否录制
    function IsFormateReady(): Boolean;   //验证
    function IsMerge(): Boolean;   //是否参与合成
    

    function GetWavFormate(var nSamplesPerSec: DWORD; var nChannels: Word; var wBitsPerSample: Word): Boolean;  //验证格式信息 并且返回
    function GetHWaveOut(): Integer;
    function GetWaveDataSize(): Integer;
    function GetStartTime(): TDateTime;

    procedure SetOutPutInfo(Path :string ; Pid : DWORD; HWaveOut : Integer); //设置输出信息

    procedure Start;   //将建立temp文件  stateRecording为1 首次调用WriteTempData是自动调用
    procedure Pause;
    procedure Recording;
    procedure Stop;      //将关闭temp文件
    function MakeAudioFile(): Boolean;  //生成wav
    procedure DeleteTemp();
  end;

implementation

{ TWaveAdapter }

procedure TWaveMaker.WriteTempData(Buf: pointer; Len: dword);
begin
  if not StateStart then
  begin
    Start;
    StartTime:= Now;
  end;  
  StateWriting:= True;
  WaveDataSize:= WaveDataSize + tempFileData.Write(Buf^ ,Len);
  StateWriting:= False;
end;

procedure TWaveMaker.SetWavFormate(WHead: tWAVEFORMATEX);
begin
  WavFormate:= WHead;
  StateFormateReady:= True;
end;

procedure TWaveMaker.SetWavInfo(nSamplesPerSec: DWORD; nChannels,
  wBitsPerSample: Word);
begin
  WavFormate.wFormatTag := WAVE_FORMAT_PCM;
  WavFormate.nChannels := nChannels;
  WavFormate.nSamplesPerSec := nSamplesPerSec;
  WavFormate.wBitsPerSample := wBitsPerSample;

  WavFormate.nBlockAlign := wBitsPerSample*nChannels div 8;
  WavFormate.nAvgBytesPerSec := nSamplesPerSec * nChannels * wBitsPerSample div 8;
  WavFormate.cbSize := 0;

  StateFormateReady:= True;
end;

function TWaveMaker.IsStart(): Boolean;
begin
  Result:= StateStart;
end;

function TWaveMaker.IsRecording(): Boolean;
begin
  Result:= StateRecording;
end;

function TWaveMaker.IsFormateReady(): Boolean;
begin
  Result:= StateFormateReady;
end;

function TWaveMaker.IsMerge: Boolean;
begin
  Result:= StateMerge;
end;

function TWaveMaker.GetWavFormate(var nSamplesPerSec: DWORD; var nChannels: Word; var wBitsPerSample: Word): Boolean;
begin
  if StateFormateReady then
  begin
    nSamplesPerSec := WavFormate.nSamplesPerSec;
    nChannels := WavFormate.nChannels;
    wBitsPerSample := WavFormate.wBitsPerSample;
  end;  
  Result:= StateFormateReady;
end;

procedure TWaveMaker.SetOutPutInfo(Path :string ; Pid : DWORD; HWaveOut : Integer);
begin
  OutPutPath:= Path+'\'+inttostr(Pid)+'_'+inttostr(HWaveOut);
end;  

procedure TWaveMaker.Start;
begin
  tempFileData := TFileStream.Create(OutPutPath+'.wavtemp', fmCreate);
  tempFileData.Seek(44,soFromBeginning);

  StateStart:= True;
  StateWriting:= False;
  WaveDataSize:= 0;

end;

procedure TWaveMaker.Pause;
begin
  while StateWriting do ;
  StateRecording:= False;
end;

procedure TWaveMaker.Recording;
begin
  StateRecording:= True;
end;

procedure TWaveMaker.Stop;
begin
  while StateWriting do ;
  StateRecording:= False;
  StateStart:= False;
  StateMerge:= True;
  FreeAndNil(tempFileData);
end;

function TWaveMaker.MakeAudioFile(): Boolean;
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
begin
  if FileExists(OutPutPath+'.wavtemp') and (StateFormateReady =True) and (WaveDataSize <> 0) then
  begin

  wh.Riff_ckid := FOURCC_RIFF;
  wh.Riff_cksize := 36 + WaveDataSize;////////////////////
  wh.Riff_fccType := mmioStringToFOURCC('WAVE', 0);
  wh.fmt_ckid := mmioStringToFOURCC('fmt', 0);
  wh.fmt_cksize := 16;
  wh.wFormatTag := WavFormate.wFormatTag;
  wh.nChannels := WavFormate.nChannels;
  wh.nSamplesPerSec := WavFormate.nSamplesPerSec;
  wh.nAvgBytesPerSec := WavFormate.nAvgBytesPerSec;
  wh.nBlockAlign := WavFormate.nBlockAlign;
  wh.wBitsPerSample := WavFormate.wBitsPerSample;
  wh.data_ckid := mmioStringToFOURCC('data', 0);
  wh.data_cksize := WaveDataSize;

    Stop;
    tempFileData := TFileStream.Create(OutPutPath+'.wavtemp', fmOpenReadWrite);
    tempFileData.Seek(0,soFromBeginning);
    tempFileData.Write(wh, SizeOf(TWaveHeader));

    FreeAndNil(tempFileData);

    RenameFile(OutPutPath+'.wavtemp',OutPutPath+'.wav') ;
    Result:= True;
  end
  else
    Result:= False;
end;

procedure TWaveMaker.DeleteTemp;
begin
  Stop;
  if FileExists(OutPutPath+'.wavtemp') then
  DeleteFile(OutPutPath+'.wavtemp');
end;

constructor TWaveMaker.Create(Path :string ; Pid : DWORD; HWaveOut : Integer);
begin
  //inherited;

  TargetPid:= Pid;
  TargetHWaveOut:= HWaveOut;

  SetOutPutInfo(Path , Pid , HWaveOut);

  StateStart:= False;
  StateRecording:= False;
  StateWriting:= False;
  StateFormateReady:= False;
  StateMerge:= False;
  WaveDataSize:= 0;
end;


function TWaveMaker.GetHWaveOut: Integer;
begin
Result:= TargetHWaveOut;
end;

function TWaveMaker.GetWaveDataSize: Integer;
begin
Result:= WaveDataSize;
end;

function TWaveMaker.GetStartTime: TDateTime;
begin
  Result:= StartTime;
end;


procedure TWaveMaker.setMerge(b: Boolean);
begin
  StateMerge:= b;
end;

end.
 