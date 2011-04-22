unit ProcessWaveCaptureFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FFBaseComponent, FFEncode, FFLog,
  ProcessWaveCapture;

type
  TfrmWaveCapture = class(TForm)
    Label2: TLabel;
    lblStatus: TLabel;
    btnOpen: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnPause: TButton;
    btnResume: TButton;
    btnWebSite: TButton;
    cboLogLevel: TComboBox;
    txtOutput: TStaticText;
    mmoLog: TMemo;
    FFEncoder: TFFEncoder;
    FFLogger: TFFLogger;
    SaveDialog1: TSaveDialog;
    Edit1: TEdit;
    btnInject: TButton;
    Edit2: TEdit;
    btnAskFmt: TButton;
    lblFmt: TLabel;
    btnVoice: TButton;
    btnNoVoice: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnOpenClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnResumeClick(Sender: TObject);
    procedure btnWebSiteClick(Sender: TObject);
    procedure cboLogLevelChange(Sender: TObject);
    procedure FFEncoderProgress(Sender: TObject; AProgressInfo: PProgressInfo);
    procedure FFEncoderTerminate(Sender: TObject; const ATerminateInfo: TTerminateInfo);
    procedure FFLoggerLog(Sender: TObject; AThreadID: Cardinal;
      ALogLevel: TLogLevel; const ALogMsg: string);
    procedure btnInjectClick(Sender: TObject);
    procedure btnAskFmtClick(Sender: TObject);
    procedure btnVoiceClick(Sender: TObject);
    procedure btnNoVoiceClick(Sender: TObject);
  private
    { Private declarations }
    FWavePauseFlag: Integer;
  public
    { Public declarations }
  end;

var
  frmWaveCapture: TfrmWaveCapture;

implementation

{$R *.dfm}

uses
{$IF (DEFINED(VER150) OR DEFINED(VER170) OR DEFINED(VER180))}
  XPMan,
{$IFEND}
  ShellAPI,
  MyUtils;

const
  CLibAVPath = 'LibAV';

  SAppTitle = 'Demo of FFEncoder %s';
  SCaption = 'Demo of FFEncoder %s (WaveCapture) - Delphi FFmpeg VCL Components';
  SWebSiteC = 'http://www.CCAVC.com';
  SWebSiteE = 'http://www.DelphiFFmpeg.com';

  // License key sample (this is a fake license key)
  // The full source edition does not need license key.
  // FFLogger does not need license key.
  // FFDecoder can accept encoder key or player key or full key.
  // FFEncoder can accept encoder key or full key.
  // FFPlayer can accept player key or full key.
//BOMB: you should replace the license key with your own one.
  LICENSE_KEY = 'FSXXXXXX-XXXXXXXX-XXXXXXXX-XXXXXXXX-XXXXXXXX';

  CAudioFiles = 'MP3|*.MP3|AAC|*.AAC|WAV|*.WAV|FLAC|*.FLAC';

var
  SWebSite: string = SWebSiteE;

procedure TfrmWaveCapture.FormCreate(Sender: TObject);
begin
  Application.Title := Format(SAppTitle, [FFEncoder.Version]);
  Self.Caption := Format(SCaption, [FFEncoder.Version]);

  if SysUtils.SysLocale.PriLangID = LANG_CHINESE then
    SWebSite := SWebSiteC
  else
    SWebSite := SWebSiteE;

  mmoLog.Text := SWebSite + #13#10#13#10;
  btnWebsite.Hint := SWebSite;
  btnWebsite.ShowHint := True;

  // init input parameters
  Edit1.Text:= Format('r=44100;c=2;f=16;pause_pointer=%d;voice=1;pid=',
                         [Integer(@FWavePauseFlag)]);

  // save dialog setting
  SaveDialog1.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
  SaveDialog1.Filter := CAudioFiles;
  SaveDialog1.DefaultExt := 'mp3';

  // Set License Key
  // The full source edition does not need license key.
  FFEncoder.SetLicenseKey(LICENSE_KEY);
end;

procedure TfrmWaveCapture.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  with FFEncoder do
  begin
    // Clear the event handlers
    OnProgress := nil;
    OnTerminate := nil;

    // Break converting
    Stop;
  end;
  FFLogger.OnLog := nil;
end;

procedure TfrmWaveCapture.btnOpenClick(Sender: TObject);
begin
  // Load dynamic link libraries
  if not FFEncoder.AVLibLoaded then
  begin
    // TPathFileName = type WideString;
    // FFEncoder.LoadAVLib(const APath: TPathFileName): Boolean;
    // APath: Full path indicates location of FFmpeg DLLs.
    //        It can be empty, let Windows search DLLs in current dir or environment <PATH>
    //if not FFEncoder.LoadAVLib(ExtractFilePath(Application.ExeName) + CLibAVPath) then
    // the routine ExePath() is implemented in unit MyUtils which returns WideString type
    // of ExtractFilePath(Application.ExeName)
    if not FFEncoder.LoadAVLib(ExePath + CLibAVPath) then
    begin
      mmoLog.Lines.Add(FFEncoder.LastErrMsg);
      Exit;
    end;

    // register wave capture demuxer
    ProcessWaveCapture.register_processwavecapture;
    //WaveCapture.register_wavecapture;
  end;

  // output filename
  if SaveDialog1.FileName = '' then
    SaveDialog1.FileName := 'wave_capture';
  if SaveDialog1.Execute then
  begin
    txtOutput.Caption := SaveDialog1.FileName;
    btnStart.Enabled := True;
    btnStart.SetFocus;
  end;


end;

procedure TfrmWaveCapture.btnStartClick(Sender: TObject);
var
  LIndex: Integer;
  IO: TInputOptions;
  OO: TOutputOptions;
  LOutFileName: string;
begin

  // ensure reset FFEncoder
  FFEncoder.ClearTasks;

  // output filename
  LOutFileName := SaveDialog1.FileName;

  // set input options
  InitInputOptions(@IO);
  IO.ForceFormat := 'processwavecapture'; //process wave capture format

  FWavePauseFlag := 0;

  // try to open input file
  // process wave capture parameters
  // filename format: <option1>=<param1>;<option2>=<param2>;...
  //  TODO: pid=int: pid of target process, default 0
  //  r=int: sample rate, default 44100 of (8000, 11025, 22050, 44100)
  //  c=int: channels, default 2 of (1, 2)
  //  f=int: sample format, default 16 of (8, 16)
  //  voice=boolean: target audio enable(1) or disable(0), default 1
  //  pause_pointer=int: integer value of pause flag pointer

  LIndex := FFEncoder.AddTask(Edit1.Text, @IO);
  if LIndex < 0 then
  begin // open failed
    mmoLog.Lines.Add('');
    mmoLog.Lines.Add('***File open error: ' + FFEncoder.LastErrMsg);
    mmoLog.Lines.Add('');
    Exit;
  end;

  // here you can get input file info by property Decoders
  // property Decoders[TaskIndex, FileIndex: Integer]: TFFDecoder read GetDecoders;
  mmoLog.Lines.Add('');
  mmoLog.Lines.Add(FFEncoder.Decoders[LIndex, 0].FileInfoText);

  // set output options
  InitOutputOptions(@OO);

  // audio output options
  OO.AudioBitrate := 128 * 1000; // 128K

  // try to set output file with output options
  if not FFEncoder.SetOutputFile(LIndex, LOutFileName, @OO) then
  begin // cannot do output file, remove input file
    FFEncoder.RemoveTask(LIndex);
    mmoLog.Lines.Add('');
    mmoLog.Lines.Add('***Cannot do convert, error: ' + FFEncoder.LastErrMsg);
    mmoLog.Lines.Add('');
    Exit;
  end;

  // can do output file with output options
  mmoLog.Lines.Add('');
  mmoLog.Lines.Add('***Can do convert.');
  mmoLog.Lines.Add('');

  // set status of buttons
  btnOpen.Enabled := False;
  btnStart.Enabled := False;
  btnStop.Enabled := True;
  btnPause.Enabled := True;
  btnResume.Enabled := False;
  btnStop.SetFocus;

  // procedure Start(AThreadCount: Integer);
  // AThreadCount: >  0, means do converting task in thread mode
  //               >  1, means do converting task with multiple files in the same time
  //               <= 0, means do converting task in main thread
  FFEncoder.Start(1);
end;

procedure TfrmWaveCapture.btnStopClick(Sender: TObject);
begin
  btnStop.Enabled := False;
  FFEncoder.Stop; // only works in thread mode
end;

procedure TfrmWaveCapture.btnPauseClick(Sender: TObject);
begin
  btnPause.Enabled := False;
  btnResume.Enabled := True;
//  FFEncoder.Pause; // only works in thread mode
  FWavePauseFlag := 1; // used for wave capture
end;

procedure TfrmWaveCapture.btnResumeClick(Sender: TObject);
begin
  btnPause.Enabled := True;
  btnResume.Enabled := False;
//  FFEncoder.Resume; // only works in thread mode
  FWavePauseFlag := 0; // used for wave capture
end;

procedure TfrmWaveCapture.btnWebSiteClick(Sender: TObject);
  function FromEXE: string;
  var
    S: string;
  begin
    S := ChangeFileExt(ExtractFileName(Application.ExeName), '');
    S := StringReplace(S, '[', '', [rfReplaceAll]);
    S := StringReplace(S, ']', '', [rfReplaceAll]);
    S := StringReplace(S, ' ', '_', [rfReplaceAll]);
    Result := '/?from=exe_' + S;
  end;
begin
  ShellExecute(Application.Handle, 'Open',   {Do not Localize}
    PChar(LowerCase(SWebSite + FromEXE)), '',
    PChar(ExtractFilePath(Application.ExeName)), 1);
end;

procedure TfrmWaveCapture.cboLogLevelChange(Sender: TObject);
begin
  FFLogger.LogLevel := TLogLevel(cboLogLevel.ItemIndex);
end;

procedure TfrmWaveCapture.FFEncoderProgress(Sender: TObject;
  AProgressInfo: PProgressInfo);
begin
  // OnProgress event handler
{
  PProgressInfo = ^TProgressInfo;
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
  end;
}
  with AProgressInfo^ do
    lblStatus.Caption := Format('Frame number: %d; FPS: %d; Size: %d; Time: %d',
        [FrameNumber, FPS, CurrentSize, CurrentDuration div 1000000]);
end;

procedure TfrmWaveCapture.FFEncoderTerminate(Sender: TObject;
  const ATerminateInfo: TTerminateInfo);
begin
  // OnTerminate event handler
{
  TTerminateInfo = record
    TaskIndex: Integer;     // index of converting tasks, (-1) means all tasks are terminated
    Finished: Boolean;      // True means converted success, False means converting breaked
    Exception: Boolean;     // True means Exception occured, False please ignore
    ExceptionMsg: string;   // Exception message
  end;
}
  if ATerminateInfo.TaskIndex < 0 then
  begin
    // set status of buttons
    btnOpen.Enabled := True;
    btnStart.Enabled := False;
    btnStop.Enabled := False;
    btnPause.Enabled := False;
    btnResume.Enabled := False;
  end;
  if ATerminateInfo.Exception then // Exception occured, show exception message
    Application.MessageBox(PChar(ATerminateInfo.ExceptionMsg), PChar(Application.Title), MB_OK + MB_ICONWARNING);
end;

procedure TfrmWaveCapture.FFLoggerLog(Sender: TObject; AThreadID: Cardinal;
  ALogLevel: TLogLevel; const ALogMsg: string);
begin
  // OnLog event handler
  // TLogLevel = (llQuiet, llPanic, llFatal, llError, llWarning, llInfo, llVerbose, llDebug);
  mmoLog.Lines.Add('#' + IntToStr(AThreadID) + '.' + IntToStr(Ord(ALogLevel)) + ': ' + ALogMsg);
end;

procedure TfrmWaveCapture.btnInjectClick(Sender: TObject);
begin
  if ProcessWaveCapture.InjectTarget(StrToInt(Edit2.Text)) then
  Edit1.Text:= Edit1.Text + Edit2.Text;
end;

procedure TfrmWaveCapture.btnAskFmtClick(Sender: TObject);
begin
  lblFmt.Caption:= ProcessWaveCapture.AskFormat(StrToInt(Edit2.Text));
end;

procedure TfrmWaveCapture.btnVoiceClick(Sender: TObject);
begin
  ProcessWaveCapture.SendVoice(StrToInt(Edit2.Text), True);
end;

procedure TfrmWaveCapture.btnNoVoiceClick(Sender: TObject);
begin
  ProcessWaveCapture.SendVoice(StrToInt(Edit2.Text), False);
end;

end.
