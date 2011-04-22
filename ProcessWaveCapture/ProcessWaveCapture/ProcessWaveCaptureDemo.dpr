program ProcessWaveCaptureDemo;

uses
  Forms,
  ProcessWaveCaptureFrm in 'ProcessWaveCaptureFrm.pas' {frmWaveCapture},
  ProcessWaveCapture in 'ProcessWaveCapture.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmWaveCapture, frmWaveCapture);
  Application.Run;
end.
