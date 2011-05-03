unit uCaptureTypes;

interface
uses
  Windows, Types;
type

  TGrabMode = (gmDC, gmPW); (* The grab mode, gmDC use GetDC and gmPW use printwindow function*)
  PCaptureForm = ^TCaptureForm;
  TCaptureForm = record
    Handle: HWND;
    Left, Top: Integer;
    GrabMode: TGrabMode;
    ShowFrame: Integer;
  end;

  PVideoInputOption = ^TVideoInputOption;
  TVideoInputOption = record
    Handle: HWND;
    x_off, y_off: Integer;
    Width, Height: Integer;
    Client: Integer;     // client=1: capture client dc instead of window dc
    Cursor: Integer;     // cursor=1: grab cursor
    ShowFrame: Integer;  // showframe=1: show flash frame
    GrabMode: TGrabMode;
    FrameRate: string;   // framerate=int/int: Numerator/Denominator, e.g. 30000/1001 (-> 29.97)
  end;

  PAudioInputOption = ^TAudioInputOption;
  TAudioInputOption = record
    PID: Cardinal;           //not needed in Web mode
    sample_rate: Integer;
    channels: Integer;
    sample_format: Integer;
    Voice: Integer;
  end;
  
  procedure InitVideoInputOption(PVIO: PVideoInputOption);
  procedure InitAudioInputOption(PAIO: PAudioInputOption);

implementation
procedure InitAudioInputOption(PAIO: PAudioInputOption);
begin
  PAIO.PID := 0;
  PAIO.sample_rate := 44100;
  PAIO.channels := 2;
  PAIO.sample_format := 16;
  PAIO.Voice := 1;
end;

procedure InitVideoInputOption(PVIO: PVideoInputOption);
begin
  PVIO.Handle := 0;
  PVIO.x_off := 0;
  PVIO.y_off := 0;
  PVIO.Width := 0;
  PVIO.Height := 0;
  PVIO.Client := 0;
  PVIO.Cursor := 0;
  PVIO.ShowFrame := 0;
  PVIO.GrabMode := gmDC;
  PVIO.FrameRate := '15/1';
end;

end.
