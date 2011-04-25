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
  
  procedure InitVideoInputOption(VIO: PVideoInputOption);

implementation

procedure InitVideoInputOption(VIO: PVideoInputOption);
begin
  VIO.Handle := 0;
  VIO.x_off := 0;
  VIO.y_off := 0;
  VIO.Width := 0;
  VIO.Height := 0;
  VIO.Client := 0;
  VIO.Cursor := 0;
  VIO.ShowFrame := 0;
  VIO.GrabMode := gmDC;
  VIO.FrameRate := '15/1';
end;

end.
