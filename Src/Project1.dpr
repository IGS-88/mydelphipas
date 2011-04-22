program Project1;

{%ToDo 'Project1.todo'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uScreenCapture in 'uScreenCapture.pas',
  WaveMaker in 'WaveMaker.pas',
  AudioCapture in 'AudioCapture.pas',
  AudioTask in 'AudioTask.pas',
  uMerger in 'uMerger.pas',
  ScreenCapture in '..\Pas¿â\ScreenCapture.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
