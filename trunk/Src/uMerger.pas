{*******************************************************}
{                                                       }
{       TMerger                                         }
{                                                       }
{       ��Ȩ���� (C) 2011 Codeup                        }
{                                                       }
{*******************************************************}

(*
 *  ʹ��FFVCL������Ƶ��Ƶ�ϳɵ���ʵ�֡�
 *  2011-04-08 Created ʵ�ֻ������ܣ����������ʽSetOutputOptionû����ɣ�
 *             ����UseDefaultOO���档
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
    FEncode: TFFEncoder;  //FFEncoderʵ����ʵ�ֱ���ͽ��빤�������Ķ���
    FLibAVPath: string;   //Dlls ��·�������޸ģ�Ĭ��Ϊ��ִ���ļ�����λ�õ�LibAV�ļ����ڡ�
    FIO: TInputOptions;   //InputOption��ֻ����ʼ������
    FOO: TOutputOptions;  //OutputOptionʵ���������ʽ���ã���ʹ��UseDefaultOO����Ĭ�ϳ�ʼ����
    LIndex: Integer;      //AddTask��õ���������

    ID: string; //GUID

    FInputVideo: string;  //Merger�е���Ƶ�������ļ�
    FInputAudio: string;  //Merger�е���Ƶ�������ļ�
    FOutputVideo: string; //Merger����Ƶ���λ��

    FOnTerminate: TTerminateEvent;
    FOnProgress: TProgressEvent;

    procedure doLog(level: TLogLevel; msg: string); //�򻯺��Log����������uLogger
    function  CheckPrepare(OutputVideo: string): Boolean; //����Ƿ���Կ�ʼMerger
    function  LoadAVLib: Boolean; //ģ�黯��Dll�����жϣ����û�м������Զ����ء�

    procedure DoProgress(Sender: TObject; AProgressInfo: PProgressInfo);
    procedure DoTerminate(Sender: TObject; const ATerminateInfo: TTerminateInfo);
  protected

  public
    VideoStreamID: Integer; //Mergerʹ�õ���Ƶ����Ĭ��Ϊ0��Ĭ��ֵ�����ڴ���Ƶ�ļ���
    AudioStreamID: Integer; //Mergerʹ�õ���Ƶ����Ĭ��Ϊ0��Ĭ��ֵ�����ڴ���Ƶ�ļ���

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure AddVideo(InputVideo: string);
    procedure AddAudio(InputAudio: string);
    function  SetOutputOption(): Boolean;//TODO
    procedure UseDefaultOO;
    function  GetFirstVideoStream(VideoFile: string): Integer; //��ȡ�ļ��еĵ�һ����Ƶ���ı�ţ��ɸ�ֵ��VideoStreamID
    function  GetFirstAudioStream(AudioFile: string): Integer; //��ȡ�ļ��еĵ�һ����Ƶ���ı�ţ��ɸ�ֵ��AudioStreamID
  published
    { FFEncoder ������ }
    property  LibAVPath: string read FLibAVPath write FLibAVPath; //Dlls������λ��
    property  GUID: string read ID;
    property  OnTerminate: TTerminateEvent read FOnTerminate write FOnTerminate;
    property  OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    function  Start(OutputVideo: string): Boolean;
    procedure Stop;
  end;

const
  Flag_Uninit = 'UnInit'; //δ����ı�־
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
  ������:    TMerger.CheckPrepare
  ����:      Arthur
  ����:      2011.04.08
  ����:      OutputVideo: string
  ����ֵ:    Boolean
  ��� Startǰ��׼��������InputVideo InputAudio OutputOption OutputVideo
  ��� OutputVideo���ã���Merger.FoutputVideo ��ֵΪOutputVideo
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
  ������:    TMerger.LoadAVLib
  ����:      Arthur
  ����:      2011.04.08
  ����:      ��
  ����ֵ:    Boolean
  ����Ƿ��Ѿ�װ��DLL��δװ����װ��
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
  ������:    TMerger.UseDefaultOO
  ����:      Arthur
  ����:      2011.04.08
  ����:      ��
  ����ֵ:    ��
  ʹ�� Ĭ�ϵ�OutputOption
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
