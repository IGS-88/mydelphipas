unit uScreenCapture;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  uCaptureTypes;

procedure register_screencapture;
implementation

uses
  libavcodec,
  AVCodecStubs,
  libavformat,
  AVFormatStubs,
  libavutil,
  libavutil_error,
  libavutil_log,
  libavutil_pixfmt,
  libavutil_rational,
  AVUtilStubs,

  FFUtils,
  uLogger,
  MyUtils;

const
  CFrameBorder = 2;
  CFrameColors: array[0..1] of TColor = (clRed, clBlue);

type

  TFrameForm = class;
  Pwin32_grab = ^Twin32_grab;
  Twin32_grab = record
//    window_handle: HWND;    (* handle of the window for the grab *)
    source_hdc: HDC;        (* Source device context *)
    window_hdc: HDC;        (* Destination, source-compatible device context *)
    hbmp: HBITMAP;          (* Information on the bitmap captured *)
    //=======================================================
    Mem_hdc: HDC;           (* HDC in Memory, the same as window_hdc. PrintWindow to Mem_hdc *)//Add at 2011/3/17 by Codeup
    Hbmp_Mem: HBITMAP;      (* HBITMAP for Mem_hdc, PrintWindow WindowScreen to Mem_hdc, which selected with HBmp_Mem *)
    ParentGUID: string;     (* GUID of Parent TScreenCapture. Send this param to Log *)
    CaptureForm: PCaptureForm;
    Mutex: THandle;         (* *)
    //=======================================================
    time_base: TAVRational; (* Time base *)
    time_frame: Int64;      (* Current time *)
    time_start: Int64;
    Started: Integer;

//    x_off: Integer;         (* Horizontal top-left corner coordinate *)
//    y_off: Integer;         (* Vertical top-left corner coordinate *)
    cursor: Integer;        (* Also capture cursor *)

    size: Integer;          (* Size in bytes of the grab frame *)
//    width: Integer;         (* Width of the grab frame *)
//    height: Integer;        (* Height of the grab frame *)
    bpp: Integer;           (* Bits per pixel of the grab frame *)

    client: Integer;        // only capture client of window

//    show_frame: PInteger;    // show flashing frame
    frame: TFrameForm;      // frame form
  end;

  TFlashThread = class(TThread)
  private
    FOwner: TFrameForm;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFrameForm);
  end;

  TFrameForm = class(TCustomForm)
  private
    Fwin32_grab: Pwin32_grab;
    FBorder: Integer;
    FThread: TFlashThread;
    FLock: Integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(Awin32_grab: Pwin32_grab); reintroduce;
    destructor Destroy; override;
    procedure AdjustPosition;
    procedure Flash;
  end;

function PrintWindow(SourceWindow: hwnd; Destination: hdc; nFlags: cardinal): bool; stdcall; external 'user32.dll' name 'PrintWindow';

function GetCFRect(const Awin32_grab: Pwin32_grab; const ABorder: Integer; var ShowFrame: Integer): TRect;
var
  R: TRect;
  window_handle: HWND;
  x_off, y_off: Integer;
  width, height: Integer;
begin
  if WaitForSingleObject(Awin32_grab.Mutex, INFINITE) = WAIT_OBJECT_0 then
  begin
    window_handle := Awin32_grab.CaptureForm.Handle;
    x_off := Awin32_grab.CaptureForm.Left;
    y_off := Awin32_grab.CaptureForm.Top;
    width := Awin32_grab.CaptureForm.Width;
    height := Awin32_grab.CaptureForm.Height;
    ShowFrame := Awin32_grab.CaptureForm.ShowFrame;
  end;
  ReleaseMutex(Awin32_grab.Mutex);

  if window_handle <> 0 then
  begin
    if Awin32_grab.client <> 0 then
    begin
      Windows.GetClientRect(window_handle, R);
      Windows.ClientToScreen(window_handle, R.TopLeft);
    end
    else
      GetWindowRect(window_handle, R);
    Result.Left := R.Left + x_off - ABorder;
    Result.Top := R.Top + y_off - ABorder;
  end
  else
  begin
    Result.Left := x_off - ABorder;
    Result.Top := y_off - ABorder;
  end;
  Result.Right := Result.Left + width + ABorder * 2;
  Result.Bottom := Result.Top + height + ABorder * 2;
end;

function GetTopLeft(const Awin32_grab: Pwin32_grab; const ABorder: Integer): TPoint;
var
  R: TRect;
  window_handle: HWND;
  x_off, y_off: Integer;
begin
  if WaitForSingleObject(Awin32_grab.Mutex, INFINITE) = WAIT_OBJECT_0 then
  begin
    window_handle := Awin32_grab.CaptureForm.Handle;
    x_off := Awin32_grab.CaptureForm.Left;
    y_off := Awin32_grab.CaptureForm.Top;
  end;
  ReleaseMutex(Awin32_grab.Mutex);

  if window_handle <> 0 then
  begin
    if Awin32_grab.client <> 0 then
    begin
      Windows.GetClientRect(window_handle, R);
      Windows.ClientToScreen(window_handle, R.TopLeft);
    end
    else
      GetWindowRect(window_handle, R);
    Result.X := R.Left + x_off - ABorder;
    Result.Y := R.Top + y_off - ABorder;
  end
  else
  begin
    Result.X := x_off - ABorder;
    Result.Y := y_off - ABorder;
  end;
end;

function GrabBmp(s: Pwin32_grab): Boolean;
var
  width, height: Integer;
  x_off, y_off: Integer;
  bmp: BITMAP;
  errcode: Integer;
  errmsg: string;
  GrabMode: TGrabMode;
  window_handle: HWND;
begin
  if WaitForSingleObject(s.Mutex, INFINITE) = WAIT_OBJECT_0 then
  begin
    width := s.CaptureForm.Width;
    height := s.CaptureForm.Height;
    x_off := s.CaptureForm.Left;
    y_off := s.CaptureForm.Top;
    GrabMode := s.CaptureForm.GrabMode;
    window_handle := s.CaptureForm.Handle;
  end;
  ReleaseMutex(s.Mutex);

  if s.hbmp <> 0 then
  begin
    DeleteObject(s.hbmp);
  end;

  s.hbmp := CreateCompatibleBitmap(s.source_hdc, width, height);
  if s.hbmp = 0 then
  begin
    WriteLog(GetCurrentThreadId, s.ParentGUID, llerror, Format('Screen DC CreateCompatibleBitmap (error %d)'#10, [GetLastError]));
    Result := False;
    Exit;
  end;

  (* Get info from the bitmap *)
  FillChar(bmp, sizeof(BITMAP), 0);
  if GetObject(s.hbmp, sizeof(BITMAP), @bmp) = 0 then
  begin
    errcode := GetLastError;
    if errcode <> 0 then
    begin
      errmsg := SysErrorMessage(errcode);
      WriteLog(GetCurrentThreadId, s.ParentGUID, llerror, Format('GetObject (error %d: %s)'#10, [errcode, errmsg]));
      Result := False;
      Exit;
    end
    else
    begin
      bmp.bmType := 0;
      bmp.bmWidth := width;
      bmp.bmHeight := height;
      bmp.bmWidthBytes := width * s.bpp div 8;
      bmp.bmPlanes := 1;
      bmp.bmBitsPixel := s.bpp;
      bmp.bmBits := nil;
      {WriteLog(GetCurrentThreadId, s.ParentGUID, llWarning,
               Format('GetObject failed. Force Bitmap type %d, size %dx%dx%u, ' +
               '%u planes of width %d bytes'#10,
               [bmp.bmType, bmp.bmWidth, bmp.bmHeight, bmp.bmBitsPixel,
               bmp.bmPlanes, bmp.bmWidthBytes]));}
    end;
  end;
      {WriteLog(GetCurrentThreadId, s.ParentGUID, llDebug,
               Format('Using Bitmap type %d, size %dx%dx%u, ' +
               '%u planes of width %d bytes'#10,
               [bmp.bmType, bmp.bmWidth, bmp.bmHeight, bmp.bmBitsPixel,
               bmp.bmPlanes, bmp.bmWidthBytes]));}

  if SelectObject(s.window_hdc, s.hbmp) = 0 then
  begin
    WriteLog(GetCurrentThreadId, s.ParentGUID, llerror, Format('SelectObject (error %d)'#10, [GetLastError]));
    Result := False;
    Exit;
  end;
  s.size := bmp.bmWidthBytes * bmp.bmHeight * bmp.bmPlanes;

  if GrabMode = gmDC then
  begin
    if not BitBlt(s.window_hdc, 0, 0, width, height,
                  s.source_hdc, x_off, y_off, SRCCOPY) then
    begin
      WriteLog(GetCurrentThreadId, s.ParentGUID, llError, Format('Failed to BitBlt image (error %d)'#10, [GetLastError]));
      Result := False;
      Exit;
    end;
  end
  else
  begin
    if not PrintWindow(window_handle, s.Mem_hdc, 0) then
    begin
      WriteLog(GetCurrentThreadId, s.ParentGUID, llError, Format('PrintWindow to Mem_hdc failed (error %d)'#10, [GetLastError]));
      Result := False;
      Exit;
    end;
    if not BitBlt(s.window_hdc, 0, 0, width, height,
                  s.Mem_hdc, x_off, y_off, SRCCOPY) then
    begin
      WriteLog(GetCurrentThreadId, s.ParentGUID, llError, Format('Failed to BitBlt image (error %d)'#10, [GetLastError]));
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

{ TFlashThread }

constructor TFlashThread.Create(AOwner: TFrameForm);
begin
  inherited Create(False);
  FOwner := AOwner;
end;

procedure TFlashThread.Execute;
var
  LCounter: Integer;
begin
  LCounter := 0;
  while not Terminated do
  begin
    Inc(LCounter);
    if LCounter = 50 then
    begin
      FOwner.Flash;
      LCounter := 0;
    end;
    FOwner.AdjustPosition;
    Sleep(10);
  end;
end;

{ TFrameForm }

constructor TFrameForm.Create(Awin32_grab: Pwin32_grab);
  procedure SetupForm;
  var
    P: TPoint;
    rgn, rgn1, rgn2: HRGN;
    fwidth, fheight: Integer;
  begin
    // frame outlook
    BorderStyle := bsNone;
    FormStyle := fsStayOnTop;
    BorderIcons := [];
    Position := poDesigned;
    Color := CFrameColors[0];


    // frame bounds
    P := GetTopLeft(Awin32_grab, FBorder);
    if WaitForSingleObject(Fwin32_grab.Mutex, INFINITE) = WAIT_OBJECT_0 then
    begin
      fwidth := Fwin32_grab.CaptureForm.Width;
      fheight := Fwin32_grab.CaptureForm.Height;
    end;
    ReleaseMutex(Fwin32_grab.Mutex);
    
    SetBounds(P.X, P.Y, fwidth + 2 * FBorder, fheight + 2 * FBorder);

    // frame region
    rgn :=  CreateRectRgn(0, 0, fwidth + 2 * FBorder, fheight + 2 * FBorder);
    rgn1 := CreateRectRgn(0, 0, fwidth + 2 * FBorder, fheight + 2 * FBorder);
    rgn2 := CreateRectRgn(FBorder, FBorder, fwidth + FBorder, fheight + FBorder);
    CombineRgn(rgn, rgn1, rgn2, RGN_DIFF);
    SetWindowRgn(Handle, rgn, True);
    DeleteObject(rgn);
    DeleteObject(rgn1);
    DeleteObject(rgn2);

    // do not show in taskbar
    SetWindowLong(Handle, GWL_EXSTYLE,
      GetWindowLong(Handle, GWL_EXSTYLE) or
      WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);

    // show frame
    Visible := True;
  end;
begin
  inherited CreateNew(nil);
  FBorder := CFrameBorder;
  Fwin32_grab := Awin32_grab;
  SetupForm;
  FLock := 0;
  FThread := TFlashThread.Create(Self);
end;

procedure TFrameForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := GetDesktopWindow;
end;

destructor TFrameForm.Destroy;
begin
  FThread.Free;
  inherited Destroy;
end;

procedure TFrameForm.AdjustPosition;
var
  R: TRect;
  fwidth, fheight: Integer;
  showframe: Integer;
begin
  if FLock <> 0 then
    Exit;
  FLock := 1;
  try
    R := GetCFRect(Fwin32_grab, FBorder, showframe);
    fwidth := R.Right - r.Left;
    fheight := r.Bottom - r.Top;
    if showframe <> 0 then
      Show
    else
      Hide;
    if (Left <> R.Left) or (Top <> R.Top) then
      SetWindowPos(Handle, HWND_TOPMOST, R.Left, R.Top, fwidth, fheight,
        SWP_NOSIZE or SWP_NOACTIVATE)
    else
      SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0,
        SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
  finally
    FLock := 0;
  end;
end;

procedure TFrameForm.Flash;
const
{$J+}
  ColorIdx: Boolean = True;
{$J-}
begin
  if Fwin32_grab.Started = 1 then
  begin
    Self.Color := CFrameColors[Ord(ColorIdx)];
    ColorIdx := not ColorIdx;
  end;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
end;

function win32grab_read_header(s: PAVFormatContext; ap: PAVFormatParameters): Integer; cdecl;
var
  ctx: Pwin32_grab;
  st: PAVStream;
  input_pixfmt: TAVPixelFormat;
  screenwidth: Longint;
  screenheight: Longint;
  width: Integer;
  height: Integer;
  bmp: BITMAP;
  errcode: Integer;
  errmsg: string;
  param: string;
  N, V: string;
  title: PAnsiChar;
  show_title: Boolean;
  dim: TRect;

  window_handle: HWND;
  fwidth, fheight, ftop, fleft: Integer;
  show_frame: Integer;
  Mem_bmp: BITMAP;
begin
  ctx := s.priv_data;

  width := ap.width;
  height := ap.height;

  title := 'N/A';
  show_title := False;
  show_frame := 1;
  // screen capture parameters
  // filename format: <option1>=<param1>;<option2>=<param2>;...
  //  point_captureform=int: Point of TCaptureForm
  //  framerate=int/int: Numerator/Denominator, e.g. 30000/1001 (-> 29.97)
  //  client=1: capture client dc instead of window dc
  //  cursor=1: grab cursor
  //  parentguid=string of guid, unique Object
  //  mutex=int: the mutex for synchronize
  param := string(s.filename);
  while param <> '' do
  begin
    V := Fetch(param, ';');
    N := Fetch(V, '=');
    if SameText(N, 'point_captureform') then
      // the virtual Form for capture
      ctx.CaptureForm := Pointer(StrToIntDef(V, 0))
    else if SameText(N, 'framerate') then
    begin
      // framerate=Numerator/Denominator
      N := Fetch(V, '/');
      ap.time_base.den := StrToInt(N);
      ap.time_base.num := StrToInt(V);
    end
    else if SameText(N, 'client') then
      // capture client dc instead of window dc
      ctx.client := StrToIntDef(V, 1)
    else if SameText(N, 'cursor') then
      // capture cursor
      ctx.cursor := StrToIntDef(V, 1)
    else if SameText(N, 'parentguid') then
      // Parent GUID for log              //Add for record the parent GUID
      ctx.ParentGUID := V
    else if SameText(N, 'mutex') then
      ctx.Mutex := StrToIntDef(v, 0);
  end;

  if WaitForSingleObject(ctx.Mutex, INFINITE) = WAIT_OBJECT_0 then
  begin
    window_handle := ctx.CaptureForm.Handle;
    width := ctx.CaptureForm.Width;
    height := ctx.CaptureForm.Height;
    show_frame := ctx.CaptureForm.ShowFrame;
  end;
  ReleaseMutex(ctx.Mutex);

  if ctx.client <> 0 then
    ctx.source_hdc := GetDC(window_handle)
  else
    ctx.source_hdc := GetWindowDC(window_handle);
  if ctx.source_hdc = 0 then
  begin
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llerror, Format('Couldn''t get window DC (error %d)'#10, [GetLastError]));
    Result := AVERROR_IO;
    Exit;
  end;

  screenwidth := GetDeviceCaps(GetDC(0), HORZRES);
  screenheight := GetDeviceCaps(GetDC(0), VERTRES);
  if window_handle <> 0 then
  begin
    if ctx.client <> 0 then
      GetClientRect(window_handle, dim)
    else
      GetWindowRect(window_handle, dim);
    fwidth := dim.right - dim.left;
    fheight := dim.bottom - dim.top;
  end
  else
  begin
    fwidth := screenwidth;
    fheight := screenheight;
  end;
  if (width > 0) and (width <> fwidth) then
    fwidth := width;
  if (height > 0) and (height <> fheight) then
    fheight := height;

  if fleft + fwidth > screenwidth then
    fwidth := screenwidth - fleft;
  if ftop + fheight > screenheight then
    fheight := screenheight - ftop;

  if WaitForSingleObject(ctx.Mutex, INFINITE) = WAIT_OBJECT_0 then
  begin
    ctx.CaptureForm.Width := fwidth;
    ctx.CaptureForm.Height := fheight;
  end;
  ReleaseMutex(ctx.Mutex);

  ctx.bpp := GetDeviceCaps(ctx.source_hdc, BITSPIXEL);

  if (fwidth < 0) or (fheight < 0) or (ctx.bpp mod 8 <> 0) then
  begin
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llerror, 'Invalid properties, aborting'#10);
    Result := AVERROR_IO;
    Exit;
  end;

  ctx.window_hdc := CreateCompatibleDC(ctx.source_hdc);
  if ctx.window_hdc = 0 then
  begin
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llerror, Format('Screen DC CreateCompatibleDC (error %d)'#10, [GetLastError]));
    Result := AVERROR_IO;
    Exit;
  end;

  ctx.Mem_hdc := CreateCompatibleDC(ctx.source_hdc);
  if ctx.Mem_hdc = 0 then
  begin
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llerror, Format('Mem DC CreateCompatibleDC (error %d)'#10, [GetLastError]));
    Result := AVERROR_IO;
    Exit;
  end;

  ctx.Hbmp_Mem := CreateCompatibleBitmap(ctx.source_hdc, screenwidth, screenheight);
  if ctx.Hbmp_Mem = 0 then
  begin
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llerror, Format('Mem DC CreateCompatibleBitmap (error %d)'#10, [GetLastError]));
    Result := AVERROR_IO;
    Exit;
  end;

//  ctx.hbmp := CreateCompatibleBitmap(ctx.source_hdc, ctx.width, ctx.height);
//  if ctx.hbmp = 0 then
//  begin
//    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llerror, Format('Screen DC CreateCompatibleBitmap (error %d)'#10, [GetLastError]));
//    Result := AVERROR_IO;
//    Exit;
//  end;

  (* Get info from the bitmap *)
//  FillChar(bmp, sizeof(BITMAP), 0);
//  if GetObject(ctx.hbmp, sizeof(BITMAP), @bmp) = 0 then
//  begin
//    errcode := GetLastError;
//    if errcode <> 0 then
//    begin
//      errmsg := SysErrorMessage(errcode);
//      WriteLog(GetCurrentThreadId, ctx.ParentGUID, llerror, Format('GetObject (error %d: %s)'#10, [errcode, errmsg]));
//      Result := AVERROR_IO;
//      Exit;
//    end
//    else
//    begin
//      bmp.bmType := 0;
//      bmp.bmWidth := ctx.width;
//      bmp.bmHeight := ctx.height;
//      bmp.bmWidthBytes := ctx.width * ctx.bpp div 8;
//      bmp.bmPlanes := 1;
//      bmp.bmBitsPixel := ctx.bpp;
//      bmp.bmBits := nil;
//      WriteLog(GetCurrentThreadId, ctx.ParentGUID, llWarning,
//               Format('GetObject failed. Force Bitmap type %d, size %dx%dx%u, ' +
//               '%u planes of width %d bytes'#10,
//               [bmp.bmType, bmp.bmWidth, bmp.bmHeight, bmp.bmBitsPixel,
//               bmp.bmPlanes, bmp.bmWidthBytes]));
//    end;
//  end
//  else
//      WriteLog(GetCurrentThreadId, ctx.ParentGUID, llDebug,
//               Format('Using Bitmap type %d, size %dx%dx%u, ' +
//               '%u planes of width %d bytes'#10,
//               [bmp.bmType, bmp.bmWidth, bmp.bmHeight, bmp.bmBitsPixel,
//               bmp.bmPlanes, bmp.bmWidthBytes]));
//  if SelectObject(ctx.window_hdc, ctx.hbmp) = 0 then
//  begin
//    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llerror, Format('SelectObject (error %d)'#10, [GetLastError]));
//    Result := AVERROR_IO;
//    Exit;
//  end;
//
//  ===============================================
(* Get info from the bitmap *)
  FillChar(Mem_bmp, sizeof(BITMAP), 0);
  if GetObject(ctx.Hbmp_Mem, sizeof(BITMAP), @Mem_bmp) = 0 then
  begin
    errcode := GetLastError;
    errcode := 0;   //强制更改GetLastError的结果，不影响后续工作。
    if errcode <> 0 then
    begin
      errmsg := SysErrorMessage(errcode);
      WriteLog(GetCurrentThreadId, ctx.ParentGUID, llerror, Format('Mem GetObject (error %d: %s)'#10, [errcode, errmsg]));
      Result := AVERROR_IO;
      Exit;
    end
    else
    begin
      Mem_bmp.bmType := 0;
      Mem_bmp.bmWidth := screenwidth;
      Mem_bmp.bmHeight := screenheight;
      Mem_bmp.bmWidthBytes := screenwidth * ctx.bpp div 8;
      Mem_bmp.bmPlanes := 1;
      Mem_bmp.bmBitsPixel := ctx.bpp;
      Mem_bmp.bmBits := nil;
      WriteLog(GetCurrentThreadId, ctx.ParentGUID, llWarning,
               Format('Mem GetObject failed. Force Bitmap type %d, size %dx%dx%u, ' +
               '%u planes of width %d bytes'#10,
               [Mem_bmp.bmType, Mem_bmp.bmWidth, Mem_bmp.bmHeight, Mem_bmp.bmBitsPixel,
               Mem_bmp.bmPlanes, Mem_bmp.bmWidthBytes]));
    end;
  end
  else
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llDebug,
             Format('Mem Using Bitmap type %d, size %dx%dx%u, ' +
             '%u planes of width %d bytes'#10,
              [Mem_bmp.bmType, Mem_bmp.bmWidth, Mem_bmp.bmHeight, Mem_bmp.bmBitsPixel,
              Mem_bmp.bmPlanes, Mem_bmp.bmWidthBytes]));
  if SelectObject(ctx.Mem_hdc, ctx.Hbmp_Mem) = 0 then
  begin
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llError, Format('Mem SelectObject (error %d)'#10, [GetLastError]));
    Result := AVERROR_IO;
    Exit;
  end;
//  =================================================

  case ctx.bpp of
    8: input_pixfmt := PIX_FMT_PAL8;
    16: input_pixfmt := PIX_FMT_RGB555;
    24: input_pixfmt := PIX_FMT_BGR24;
    32: input_pixfmt := PIX_FMT_RGB32;
  else
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llError, Format('image depth %u not supported ... aborting'#10, [ctx.bpp]));
    Result := -1;
    Exit;
  end;
//  ctx.size := bmp.bmWidthBytes * bmp.bmHeight * bmp.bmPlanes;
  ctx.size := (fwidth * ctx.bpp div 8) * fheight * 1;

  st := av_new_stream(s, 0);
  if st = nil then
  begin
    Result := AVERROR_NOMEM;
    Exit;
  end;
  av_set_pts_info(st, 64, 1, 1000000); (* 64 bits pts in us *)

  if (ap.time_base.num = 0) or (ap.time_base.den = 0) then
  begin
    ap.time_base.num := 1;
    ap.time_base.den := 15;
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llInfo, Format('frame rate assume as %d/%d.'#10, [ap.time_base.num, ap.time_base.den]));
  end;

//  if ctx.show_frame <> 0 then
  ctx.frame := TFrameForm.Create(ctx);
  if show_frame <> 0 then
    ctx.frame.Show
  else
    ctx.frame.Hide;

  st.codec.codec_type := AVMEDIA_TYPE_VIDEO;
  st.codec.codec_id   := CODEC_ID_RAWVIDEO;
  st.codec.width      := fwidth;
  st.codec.height     := fheight;
  st.codec.pix_fmt    := input_pixfmt;
  st.codec.time_base  := ap.time_base;
  st.codec.bit_rate   := Round(ctx.size * 1 / av_q2d(ap.time_base) * 8);

  // !!! to avoid av_find_stream_info() to read packets
  // condition 1
  st.r_frame_rate := ctx.time_base;
  st.avg_frame_rate := ctx.time_base;
  // condition 2
  s.flags := s.flags or AVFMT_FLAG_NOPARSE;
  // condition 3
  st.first_dts := 0;
  // condition ALL
  s.probesize := 0;

  ctx.time_base  := ap.time_base;
  ctx.time_frame := Round(av_gettime / av_q2d(ap.time_base));

  if show_title then
    WriteLog(GetCurrentThreadId, ctx.ParentGUID, llInfo, Format('Found window %s, ', [title]));
  WriteLog(GetCurrentThreadId, ctx.ParentGUID, llInfo, Format('ready for capturing %ux%ux%u at (%u,%u)'#10, [fwidth, fheight, ctx.bpp, fleft, ftop]));

  Result := 0;
end;

function win32grab_read_packet(s1: PAVFormatContext; pkt: PAVPacket): Integer; cdecl;
var
  s: Pwin32_grab;
//  curtime: Int64;
  delay: Int64;
  ci: TCURSORINFO;
  icon: HICON;
  info: ICONINFO;
  x, y: Longint;
  rect: TRect;

  window_handle: HWND;
  x_off, y_off:Integer;
begin
  s := s1.priv_data;

  if s.Started = 0 then
  begin
    s.time_frame := Round(av_gettime / av_q2d(s.time_base));
    s.time_start := av_gettime;
    s.Started := 1;
  end;

  (* Calculate the time of the next frame *)
  Inc(s.time_frame, Int64(1000000));

//  curtime := 0; {stop compiler warning}
  (* wait based on the frame rate *)
  while True do
  begin
//    curtime := av_gettime;
//    delay := Round(s.time_frame * av_q2d(s.time_base)) - curtime;
    delay := Round(s.time_frame * av_q2d(s.time_base)) - av_gettime;
    if delay <= 0 then
    begin
      if delay < Int64(-1000000) * av_q2d(s.time_base) then
        Inc(s.time_frame, Int64(1000000));
      Break;
    end;
    Sleep(delay div 1000);
  end;
//  Sleep(10);

  if av_new_packet(pkt, s.size) < 0 then
  begin
    Result := AVERROR_IO;
    Exit;
  end;

//  pkt.pts := curtime;

  //============================================================
  (* Blit screen grab *)
  if not GrabBmp(s) then
  begin
    Result := AVERROR_IO;
    Exit;
  end;
  if WaitForSingleObject(s.Mutex, INFINITE) = WAIT_OBJECT_0 then
  begin
    window_handle := s.CaptureForm.Handle;
    x_off := s.CaptureForm.Left;
    y_off := s.CaptureForm.Top;
  end;
  ReleaseMutex(s.Mutex);
  //============================================================

  if s.cursor <> 0 then
  begin
    (*
    http://www.codeproject.com/csharp/DesktopCaptureWithMouse.asp?df=100&forumid=261533&exp=0&select=1442638
    *)
    ci.cbSize := sizeof(ci);

    if GetCursorInfo(ci) then
    begin
      if ci.flags = CURSOR_SHOWING then
      begin
        icon := CopyIcon(ci.hCursor);
        try
          if (icon <> 0) and GetIconInfo(icon, info) then
          begin
            x := ci.ptScreenPos.x - Longint(info.xHotspot);
            y := ci.ptScreenPos.y - Longint(info.yHotspot);

            if window_handle <> 0 then
            begin
              if ((s.client <> 0) and GetClientRect(window_handle, rect)) or
                ((s.client = 0) and GetWindowRect(window_handle, rect)) then
              begin
                if s.client <> 0 then
                begin
                  ClientToScreen(window_handle, rect.TopLeft);
                  ClientToScreen(window_handle, rect.BottomRight);
                end;
                //av_log(s1, AV_LOG_DEBUG, 'Pos(%d,%d) . (%d,%d)'#10, x, y, x - rect.left, y - rect.top);
                Dec(x, rect.left);
                Dec(y, rect.top);
              end
              else
              begin
//                av_log(s1, AV_LOG_ERROR, 'Couldn''t draw icon: %d'#10, GetLastError);
                WriteLog(GetCurrentThreadId, s.ParentGUID, llError, Format('Couldn''t draw icon: %d'#10, [GetLastError]));
                s.cursor := 0; // do not capture cursor any more
              end;
            end;

            Dec(x, x_off);
            Dec(y, y_off);
            if not DrawIcon(s.window_hdc, x, y, icon) then
            begin
//              av_log(s1, AV_LOG_ERROR, 'Couldn''t draw icon: error %d'#10, GetLastError);
              WriteLog(GetCurrentThreadId, s.ParentGUID, llError, Format('Couldn''t draw icon: error %d'#10, [GetLastError]));
              s.cursor := 0; // do not capture cursor any more
            end;
          end
          else if icon <> 0 then
          begin
//            av_log(s1, AV_LOG_ERROR, 'Couldn''t get icon info: error %d'#10, GetLastError);
            WriteLog(GetCurrentThreadId, s.ParentGUID, llError, Format('Couldn''t get icon info: error %d'#10, [GetLastError]));
            s.cursor := 0; // do not capture cursor any more
          end
          else
          begin
//            av_log(s1, AV_LOG_ERROR, 'Couldn''t copy icon: error %d'#10, GetLastError);
            WriteLog(GetCurrentThreadId, s.ParentGUID, llError, Format('Couldn''t copy icon: error %d'#10, [GetLastError]));
            s.cursor := 0; // do not capture cursor any more
          end;
        finally
          if icon <> 0 then
            DestroyIcon(icon);
        end;
      end;
    end
    else
    begin
//      av_log(s1, AV_LOG_ERROR, 'Couldn''t get cursor info: error %d'#10, GetLastError);
      WriteLog(GetCurrentThreadId, s.ParentGUID, llError, Format('Couldn''t get cursor info: error %d'#10, [GetLastError]));
      s.cursor := 0; // do not capture cursor any more
    end;
  end;

  (* Get bits *)
  if GetBitmapBits(s.hbmp, s.size, pkt.data) = 0 then
  begin
//    av_log(s1, AV_LOG_ERROR, 'GetBitmapBits failed (error %d)'#10, GetLastError);
    WriteLog(GetCurrentThreadId, s.ParentGUID, llError, Format('GetBitmapBits failed (error %d)'#10, [GetLastError]));
    Result := -1;
    Exit;
  end;

  Result := s.size;
end;

function win32grab_read_close(s: PAVFormatContext): Integer; cdecl;
var
  ctx: Pwin32_grab;
  window_handle: HWND;
begin
  ctx := s.priv_data;
  if WaitForSingleObject(ctx.Mutex, INFINITE) = WAIT_OBJECT_0 then
  begin
    window_handle := ctx.CaptureForm.Handle;
  end;
  ReleaseMutex(ctx.Mutex);

  // release resource
  if ctx.source_hdc <> 0 then
    ReleaseDC(window_handle, ctx.source_hdc);
  if ctx.window_hdc <> 0 then
    DeleteDC(ctx.window_hdc);
//====================================
  if ctx.Mem_hdc <> 0 then
    DeleteDC(ctx.Mem_hdc);
  if ctx.Hbmp_Mem <> 0 then
    DeleteObject(ctx.Hbmp_Mem);
//====================================
  if ctx.hbmp <> 0 then
    DeleteObject(ctx.hbmp);
  if ctx.source_hdc <> 0 then
    DeleteDC(ctx.source_hdc);
  if ctx.frame <> nil then
    ctx.frame.Release;

  Result := 0;
end;

var
  win32_grab_device_demuxer: TAVInputFormat = (
    name: 'screencapture';
    long_name: 'Screen capture using GDI';
    priv_data_size: SizeOf(Twin32_grab);
    read_header: win32grab_read_header;
    read_packet: win32grab_read_packet;
    read_close: win32grab_read_close;
    flags: AVFMT_NOFILE;
  );

procedure register_screencapture;
begin
  RegisterInputFormat(@win32_grab_device_demuxer);
end;

end.
