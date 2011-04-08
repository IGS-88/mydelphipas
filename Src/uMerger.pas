{*******************************************************}
{                                                       }
{       TMerger                                         }
{                                                       }
{       版权所有 (C) 2011 Codeup                        }
{                                                       }
{*******************************************************}

(*
 *  使用FFVCL进行视频音频合成的类实现。
 *  2011-04-08 Created 实现基本功能，但是输出格式SetOutputOption没有完成，
 *             暂用UseDefaultOO代替。
 *)
unit uMerger;

interface
uses
  Windows, Classes, SysUtils, Types, uLogger, FFEncode, uGUID;
type
  {PProgressInfo = ^TProgressInfo;
  TProgressInfo = record
    TaskIndex: Integer;     // index of converting tasks
    FileIndex: Integer;     // index of input files in the current task
    FrameNumber: Integer;   // current frame number
    FPS: Integer;           // video converting speed, frames per second, not valid when only audio output
    Quality: Single;        // quality
    BitRate: Single;        // bitrate
    CurrentSize: Int64;     // current output file size in bytes
    CurrentDuration: Int64; // current duration time in microsecond
    TotalDuration: Int64;   // total output duration time in microsecond
  end;}
  TProgressInfo = FFEncode.TProgressInfo;
  TProgressEvent = procedure(Sender: TObject; AProgressInfo: PProgressInfo) of object;

  {TTerminateInfo = record
    TaskIndex: Integer;     // index of converting tasks, (-1) means all tasks are terminated
    Finished: Boolean;      // True means converted success, False means converting breaked
    Exception: Boolean;     // True means Exception occured, False please ignore
    ExceptionMsg: string;   // Exception message
  end;}
  TTerminateInfo = FFEncode.TTerminateInfo;
  TTerminateEvent = procedure(Sender: TObject; const ATerminateInfo: TTerminateInfo) of object;

  {TPreviewInfo = record
    TaskIndex: Integer;     // index of converting tasks
    Bitmap: TBitmap;        // bitmap filled with the target video picture, you can save it,
                            //  or paint it on any canvas(or any other control) which you want.
    FrameNumber: Integer;   // frame index number, first is 1 not 0
    PTS: Int64;             // presentation time stamp of current picture, in microseconds
  end;}

  TMerger = class(TObject)
  private
    FEncode: TFFEncoder;  //FFEncoder实例，实现编码和解码工作，核心对象
    FLibAVPath: string;   //Dlls 的路径，可修改，默认为可执行文件所在位置的LibAV文件夹内。
    FIO: TInputOptions;   //InputOption，只做初始化操作
    FOO: TOutputOptions;  //OutputOption实例，输出格式设置，可使用UseDefaultOO进行默认初始化。
    LIndex: Integer;      //AddTask后得到的任务编号

    ID: string; //GUID

    FInputVideo: string;  //Merger中的视频流所在文件
    FInputAudio: string;  //Merger中的音频流所在文件
    FOutputVideo: string; //Merger后视频输出位置

    FOnTerminate: TTerminateEvent;
    FOnProgress: TProgressEvent;

    procedure doLog(level: TLogLevel; msg: string); //简化后的Log操作，调用uLogger
    function  CheckPrepare(OutputVideo: string): Boolean; //检查是否可以开始Merger
    function  LoadAVLib: Boolean; //模块化的Dll加载判断，如果没有加载则自动加载。

    procedure DoProgress(Sender: TObject; AProgressInfo: PProgressInfo);
    procedure DoTerminate(Sender: TObject; const ATerminateInfo: TTerminateInfo);
  protected

  public
    VideoStreamID: Integer; //Merger使用的视频流，默认为0，默认值适用于纯视频文件。
    AudioStreamID: Integer; //Merger使用的音频流，默认为0，默认值适用于纯音频文件。

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure AddVideo(InputVideo: string);
    procedure AddAudio(InputAudio: string);
    function  SetOutputOption(): Boolean;//TODO
    procedure UseDefaultOO;
    function  GetFirstVideoStream(VideoFile: string): Integer; //获取文件中的第一个视频流的编号，可赋值给VideoStreamID
    function  GetFirstAudioStream(AudioFile: string): Integer; //获取文件中的第一个音频流的编号，可赋值给AudioStreamID
  published
    { FFEncoder 的属性 }
    property  LibAVPath: string read FLibAVPath write FLibAVPath; //Dlls的所在位置
    property  GUID: string read ID;
    property  OnTerminate: TTerminateEvent read FOnTerminate write FOnTerminate;
    property  OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    function  Start(OutputVideo: string): Boolean;
    procedure Stop;
  end;

const
  Flag_Uninit = 'UnInit'; //未定义的标志
implementation
uses
  AVCodecStubs, // for avcodec_find_encoder_by_name()
  MyUtils;

{ TMerger }

procedure TMerger.AddAudio(InputAudio: string);
begin
  FInputAudio := InputAudio;
end;

procedure TMerger.AddVideo(InputVideo: string);
begin
  FInputVideo := InputVideo;
end;

{-------------------------------------------------------------------------------
  过程名:    TMerger.CheckPrepare
  作者:      Arthur
  日期:      2011.04.08
  参数:      OutputVideo: string
  返回值:    Boolean
  检查 Start前的准备工作，InputVideo InputAudio OutputOption OutputVideo
  如果 OutputVideo可用，则将Merger.FoutputVideo 赋值为OutputVideo
-------------------------------------------------------------------------------}
function TMerger.CheckPrepare(OutputVideo: string): Boolean;
begin
  Result := False;
  if FInputVideo = EmptyStr then
  begin
    doLog(llError, 'InputVideo is empty');
    Exit;
  end;

  if FInputAudio = EmptyStr then
  begin
    doLog(llError, 'InputAudio is empty');
    Exit;
  end;

  if OutputVideo = EmptyStr then
  begin
    dolog(llError, 'OutputVideo is empty');
    Exit;
  end
  else
  begin
    FOutputVideo := OutputVideo;
  end;

  if FOO.FileName = FLag_Uninit then
  begin
    dolog(llError, 'OutputOption is not initialized!');
    Exit;
  end;

  Result := True;
end;

constructor TMerger.Create(AOwner: TComponent);
begin
  FLibAVPath := ExePath + 'LibAV';
  FInputVideo := '';
  FInputAudio := '';
  FOutputVideo := '';
  AudioStreamID := 0;
  VideoStreamID := 0;
  FOO.FileName := Flag_Uninit;

  FOnProgress := nil;
  FOnTerminate := nil;
  FEncode := TFFEncoder.Create(AOwner);
  FEncode.OnProgress := DoProgress;
  FEncode.OnTerminate := DoTerminate;

  ID  := GetGUID;
end;

destructor TMerger.Destroy;
begin
  if FEncode.Working then
  begin
    FEncode.Stop();
  end;
  FEncode.Destroy;
  inherited;
end;

procedure TMerger.doLog(level: TLogLevel; msg: string);
begin
  WriteLog(GetCurrentThreadId, ID, level, msg);
end;

procedure TMerger.DoProgress(Sender: TObject;
  AProgressInfo: PProgressInfo);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, AProgressInfo);
end;

procedure TMerger.DoTerminate(Sender: TObject;
  const ATerminateInfo: TTerminateInfo);
begin
  if ATerminateInfo.TaskIndex = -1 then
    dolog(llInfo, 'Merger Stoped!');
  if Assigned(FOnTerminate) then
    FOnTerminate(Self, ATerminateInfo);
end;

function TMerger.GetFirstAudioStream(AudioFile: string): Integer;
begin
  Result := -1;
  if not LoadAVLib then
  Exit;

  if not FEncode.Decoder.LoadFile(AudioFile) then
  begin
    dolog(llError, 'Can''t Open Audio File: ' + AudioFile);
    Exit;
  end;
  Result := FEncode.Decoder.FirstAudioStreamIndex;
  FEncode.Decoder.CloseFile;

  if Result < 0 then
  doLog(llWarning, 'No Audio Stream');
end;

function TMerger.GetFirstVideoStream(VideoFile: string): Integer;
begin
  Result := -1;
  if not LoadAVLib then
  Exit;

  if not FEncode.Decoder.LoadFile(VideoFile) then
  begin
    dolog(llError, 'Can''t Open Video File: ' + VideoFile);
    Exit;
  end;
  Result := FEncode.Decoder.FirstVideoStreamIndex;
  FEncode.Decoder.CloseFile;

  if Result < 0 then
  doLog(llWarning, 'No Video Stream');
end;

{-------------------------------------------------------------------------------
  过程名:    TMerger.LoadAVLib
  作者:      Arthur
  日期:      2011.04.08
  参数:      无
  返回值:    Boolean
  检查是否已经装载DLL，未装载则装载
-------------------------------------------------------------------------------}
function TMerger.LoadAVLib: Boolean;
begin
  Result := False;
  // Load dynamic link libraries
  if not FEncode.AVLibLoaded then
  begin
    // TPathFileName = type WideString;
    // FFEncoder.LoadAVLib(const APath: TPathFileName): Boolean;
    // APath: Full path indicates location of FFmpeg DLLs.
    //        It can be empty, let Windows search DLLs in current dir or environment <PATH>
    //if not FFEncoder.LoadAVLib(ExtractFilePath(Application.ExeName) + CLibAVPath) then
    // the routine ExePath() is implemented in unit MyUtils which returns WideString type
    // of ExtractFilePath(Application.ExeName)
    if not FEncode.LoadAVLib(FLibAVPath) then
    begin
      doLog(llError, FEncode.LastErrMsg);
      Exit;
    end;
  end;
  Result := True;
end;

function TMerger.SetOutputOption: Boolean;
begin
  if not LoadAVLib then
  begin
    Exit;
  end;
{ TODO : TODO Set Output Option }
end;

function TMerger.Start(OutputVideo: string): Boolean;
begin
  Result := False;

  if not LoadAVLib then
  begin
    Exit;
  end;

  FEncode.ClearTasks;

  if not CheckPrepare(OutputVideo) then
  begin
    Exit;
  end;

  // set input options
  InitInputOptions(@FIO);
  // try to open video file
  LIndex := FEncode.AddTask(FInputVideo, @FIO);
  if LIndex < 0 then
  begin // video file open failed
    dolog(llError, '***Video file open error: ' + FEncode.LastErrMsg);
    Exit;
  end;

  // set input options
  InitInputOptions(@FIO);
  // try to open audio file
  if not FEncode.AddInputFile(LIndex, FInputAudio, @FIO) then
  begin // audio file open failed
    dolog(llError, '***Audio file open error: ' + FEncode.LastErrMsg);
    Exit;
  end;

  // try to set output file with output options
  if not FEncode.SetOutputFile(LIndex, FOutputVideo, @FOO) then
  begin // cannot do output file, remove input file
    FEncode.RemoveTask(LIndex);
    doLog(llError, '***Cannot do convert, error: ' + FEncode.LastErrMsg);
    Exit;
  end;

  FEncode.Start(1);
  doLog(llInfo, 'Start Mergering!');
  Result := True;
end;

procedure TMerger.Stop;
begin
  FEncode.Stop();
  dolog(llInfo, 'Manual Stop Merger!');
end;


{-------------------------------------------------------------------------------
  过程名:    TMerger.UseDefaultOO
  作者:      Arthur
  日期:      2011.04.08
  参数:      无
  返回值:    无
  使用 默认的OutputOption
-------------------------------------------------------------------------------}
procedure TMerger.UseDefaultOO;
begin
  if not LoadAVLib then
  begin
    Exit;
  end;

  // set output options
  InitOutputOptions(@FOO);

  // ipod mp4 output options
  // -acodec aac -vcodec mpeg4 width<=320 height<=240
  FOO.VideoCodec := 'mpeg4';     {Do not Localize}
  if Assigned(avcodec_find_encoder_by_name('libfaac')) then
    FOO.AudioCodec := 'libfaac'   {Do not Localize}
  else
    FOO.AudioCodec := 'aac';   {Do not Localize}
  FOO.FrameSize := '320x240';

  // OO.ExtOptions usage: extended options for flexibility
  // Format: name1=value1<CRLF>name2=value2<CRLF>...nameN=valueN<CRLF>
  //         name and value correspond to ffmpeg.exe's parameters
  //         e.g. "pix_fmt=yuv422p<CRLF>aspect=16:9<CRLF>"
  // you can use this options according to your special purpose

  // important options for merging video and audio
  // -map file:stream[:syncfile:syncstream]  set input stream mapping
  FOO.ExtOptions := Format('map=0:%d'#10'map=1:%d', [VideoStreamID, AudioStreamID]);

  // check aac encoder
  // encoder 'aac' is experimental and might produce bad results.
  // Add '-[a]strict experimental' if you want to use it.
  if FOO.AudioCodec = 'aac' then
    FOO.ExtOptions := 'astrict=experimental'#13#10 + FOO.ExtOptions;
end;

end.
