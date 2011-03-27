(*
 * Win32 video grab interface
 *
 * This file is part of FFmpeg.
 *
 * Copyright (C) 2007 Christophe Gisquet <christophe.gisquet <at> free.fr>
 *
 * FFmpeg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with FFmpeg; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

(**
 * @file win32grab.c
 * Win32 frame device demuxer by Christophe Gisquet
 * <christophe.gisquet <at> free.fr>
 *)

(*
 * CCAVC - CodeCoolie Audio Video Components
 * http://www.CCAVC.com
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: win32grab.c
 * Ported by CodeCoolie@CNSW 2009/08/31 -> $Date:: 2010-09-09 #$
 *)

unit ScreenCapture;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms;

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
  MyUtils;

const
  CFrameBorder = 2;
  CFrameColors: array[0..1] of TColor = (clRed, clBlue);

type
  TFrameForm = class;
  Pwin32_grab = ^Twin32_grab;
  Twin32_grab = record
    window_handle: HWND;    (* handle of the window for the grab *)
    source_hdc: HDC;        (* Source device context *)
    window_hdc: HDC;        (* Destination, source-compatible device context *)
    hbmp: HBITMAP;          (* Information on the bitmap captured *)
    //=======================================================
    Mem_hdc: HDC;           (* HDC in Memory, the same as window_hdc. PrintWindow to Mem_hdc *)//Add at 2011/3/17 by Codeup
    Hbmp_Mem: HBITMAP;      (* HBITMAP for Mem_hdc, PrintWindow WindowScreen to Mem_hdc, which selected with HBmp_Mem *)
    ParentGUID: string;     (* GUID of Parent TScreenCapture. Send this param to Log *)
    //=======================================================
    time_base: TAVRational; (* Time base *)
    time_frame: Int64;      (* Current time *)
    time_start: Int64;
    Started: Integer;

    x_off: Integer;         (* Horizontal top-left corner coordinate *)
    y_off: Integer;         (* Vertical top-left corner coordinate *)
    cursor: Integer;        (* Also capture cursor *)

    size: Integer;          (* Size in bytes of the grab frame *)
    width: Integer;         (* Width of the grab frame *)
    height: Integer;        (* Height of the grab frame *)
    bpp: Integer;           (* Bits per pixel of the grab frame *)

    client: Integer;        // only capture client of window

    show_frame: Integer;    // show flashing frame
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

function GetTopLeft(const Awin32_grab: Pwin32_grab; const ABorder: Integer): TPoint;
var
  R: TRect;
begin
  if Awin32_grab.window_handle <> 0 then
  begin
    if Awin32_grab.client <> 0 then
    begin
      Windows.GetClientRect(Awin32_grab.window_handle, R);
      Windows.ClientToScreen(Awin32_grab.window_handle, R.TopLeft);
    end
    else
      GetWindowRect(Awin32_grab.window_handle, R);
    Result.X := R.Left + Awin32_grab.x_off - ABorder;
    Result.Y := R.Top + Awin32_grab.y_off - ABorder;
  end
  else
  begin
    Result.X := Awin32_grab.x_off - ABorder;
    Result.Y := Awin32_grab.y_off - ABorder;
  end;
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
  begin
    // frame outlook
    BorderStyle := bsNone;
    FormStyle := fsStayOnTop;
    BorderIcons := [];
    Position := poDesigned;
    Color := CFrameColors[0];

    // frame bounds
    P := GetTopLeft(Awin32_grab, FBorder);
    SetBounds(P.X, P.Y, Fwin32_grab.width + 2 * FBorder, Fwin32_grab.height + 2 * FBorder);

    // frame region
    rgn :=  CreateRectRgn(0, 0, Fwin32_grab.width + 2 * FBorder, Fwin32_grab.height + 2 * FBorder);
    rgn1 := CreateRectRgn(0, 0, Fwin32_grab.width + 2 * FBorder, Fwin32_grab.height + 2 * FBorder);
    rgn2 := CreateRectRgn(FBorder, FBorder, Fwin32_grab.width + FBorder, Fwin32_grab.height + FBorder);
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
  P: TPoint;
begin
  if FLock <> 0 then
    Exit;
  FLock := 1;
  try
    P := GetTopLeft(Fwin32_grab, FBorder);
    if (Left <> P.X) or (Top <> P.Y) then
      SetWindowPos(Handle, HWND_TOPMOST, P.X, P.Y, Width, Height,
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

  Mem_bmp: BITMAP;
begin
  ctx := s.priv_data;

  width := ap.width;
  height := ap.height;

  title := 'N/A';
  show_title := False;

  // screen capture parameters
  // filename format: <option1>=<param1>;<option2>=<param2>;...
  //  hwnd=int: window handle
  //  offset=int,int: offset on x and y against the final source window
  //  framesize=int,int: width and height
  //  framerate=int/int: Numerator/Denominator, e.g. 30000/1001 (-> 29.97)
  //  showframe=1: show frame
  //  client=1: capture client dc instead of window dc
  //  cursor=1: grab cursor
  //  title=str: window caption, must be last option

  param := string(s.filename);
  while param <> '' do
  begin
    V := Fetch(param, ';', False);
    N := Fetch(V, '=');
    if SameText(N, 'title') then
    begin
      Fetch(param, '=');
      V := param;
      // find window hanble by title
      title := PAnsiChar(AnsiString(V));
      ctx.window_handle := FindWindowA(nil, title);
      //show_title := True;
      Break;
    end
    else
    begin
      Fetch(param, ';');
      if SameText(N, 'hwnd') then
        // special window hanble
        ctx.window_handle := StrToIntDef(V, 0)
      else if SameText(N, 'offset') then
      begin
        // offset=x,y
        N := Fetch(V, ',');
        ctx.x_off := StrToInt(N);
        ctx.y_off := StrToInt(V);
      end
      else if SameText(N, 'framesize') then
      begin
        // framesize=w,h
        N := Fetch(V, ',');
        width := StrToInt(N);
        height := StrToInt(V);
      end
      else if SameText(N, 'framerate') then
      begin
        // framerate=Numerator/Denominator
        N := Fetch(V, '/');
        ap.time_base.den := StrToInt(N);
        ap.time_base.num := StrToInt(V);
      end
      else if SameText(N, 'showframe') then
        // show frame
        ctx.show_frame := StrToIntDef(V, 1)
      else if SameText(N, 'client') then
        // capture client dc instead of window dc
        ctx.client := StrToIntDef(V, 1)
      else if SameText(N, 'cursor') then
        // capture cursor
        ctx.cursor := StrToIntDef(V, 1)
      else if SameText(N, 'ParentGUID') then
        ctx.ParentGUID := V;
    end;
  end;

  if ctx.client <> 0 then
    ctx.source_hdc := GetDC(ctx.window_handle)
  else
    ctx.source_hdc := GetWindowDC(ctx.window_handle);
  if ctx.source_hdc = 0 then
  begin
    av_log(s, AV_LOG_ERROR, 'Couldn''t get window DC (error %li)'#10, GetLastError);
    Result := AVERROR_IO;
    Exit;
  end;

  screenwidth := GetDeviceCaps(ctx.source_hdc, HORZRES);
  screenheight := GetDeviceCaps(ctx.source_hdc, VERTRES);
  if ctx.window_handle <> 0 then
  begin
    if ctx.client <> 0 then
      GetClientRect(ctx.window_handle, dim)
    else
      GetWindowRect(ctx.window_handle, dim);
    ctx.width := dim.right - dim.left;
    ctx.height := dim.bottom - dim.top;
  end
  else
  begin
    ctx.width := screenwidth;
    ctx.height := screenheight;
  end;
  if (width > 0) and (width <> ctx.width) then
    ctx.width := width;
  if (height > 0) and (height <> ctx.height) then
    ctx.height := height;

  if ctx.x_off + ctx.width > screenwidth then
    ctx.width := screenwidth - ctx.x_off;
  if ctx.y_off + ctx.height > screenheight then
    ctx.height := screenheight - ctx.y_off;

  ctx.bpp := GetDeviceCaps(ctx.source_hdc, BITSPIXEL);

  if (ctx.width < 0) or (ctx.height < 0) or (ctx.bpp mod 8 <> 0) then
  begin
    av_log(s, AV_LOG_ERROR, 'Invalid properties, aborting'#10);
    Result := AVERROR_IO;
    Exit;
  end;

  ctx.window_hdc := CreateCompatibleDC(ctx.source_hdc);
  if ctx.window_hdc = 0 then
  begin
    av_log(s, AV_LOG_ERROR, 'Screen DC CreateCompatibleDC (error %li)'#10, GetLastError);
    Result := AVERROR_IO;
    Exit;
  end;
  
//  ====================================================
  ctx.Mem_hdc := CreateCompatibleDC(ctx.source_hdc);
  if ctx.Mem_hdc = 0 then
  begin
    av_log(s, AV_LOG_ERROR, 'Mem DC CreateCompatibleDC (error %li)'#10, GetLastError);
    Result := AVERROR_IO;
    Exit;
  end;

  ctx.Hbmp_Mem := CreateCompatibleBitmap(ctx.source_hdc, screenwidth, screenheight);
  if ctx.Hbmp_Mem = 0 then
  begin
    av_log(s, AV_LOG_ERROR, 'Mem DC CreateCompatibleBitmap (error %li)'#10, GetLastError);
    Result := AVERROR_IO;
    Exit;
  end;
//  ===================================================

  ctx.hbmp := CreateCompatibleBitmap(ctx.source_hdc, ctx.width, ctx.height);
  if ctx.hbmp = 0 then
  begin
    av_log(s, AV_LOG_ERROR, 'Screen DC CreateCompatibleBitmap (error %li)'#10, GetLastError);
    Result := AVERROR_IO;
    Exit;
  end;

  (* Get info from the bitmap *)
  FillChar(bmp, sizeof(BITMAP), 0);
  if GetObject(ctx.hbmp, sizeof(BITMAP), @bmp) = 0 then
  begin
    errcode := GetLastError;
    if errcode <> 0 then
    begin
      errmsg := SysErrorMessage(errcode);
      av_log(s, AV_LOG_ERROR, 'GetObject (error %li: %s)'#10, errcode, PAnsiChar(AnsiString(errmsg)));
      Result := AVERROR_IO;
      Exit;
    end
    else
    begin
      bmp.bmType := 0;
      bmp.bmWidth := ctx.width;
      bmp.bmHeight := ctx.height;
      bmp.bmWidthBytes := ctx.width * ctx.bpp div 8;
      bmp.bmPlanes := 1;
      bmp.bmBitsPixel := ctx.bpp;
      bmp.bmBits := nil;
      av_log(s, AV_LOG_WARNING,
             'GetObject failed. Force Bitmap type %li, size %lix%lix%i, ' +
             '%i planes of width %li bytes'#10,
             bmp.bmType, bmp.bmWidth, bmp.bmHeight, bmp.bmBitsPixel,
             bmp.bmPlanes, bmp.bmWidthBytes);
    end;
  end
  else
    av_log(s, AV_LOG_DEBUG,
           'Using Bitmap type %li, size %lix%lix%i, ' +
           '%i planes of width %li bytes'#10,
           bmp.bmType, bmp.bmWidth, bmp.bmHeight, bmp.bmBitsPixel,
           bmp.bmPlanes, bmp.bmWidthBytes);
  if SelectObject(ctx.window_hdc, ctx.hbmp) = 0 then
  begin
    av_log(s, AV_LOG_ERROR, 'SelectObject (error %li)'#10, GetLastError);
    Result := AVERROR_IO;
    Exit;
  end;

//  ===============================================
(* Get info from the bitmap *)
  FillChar(Mem_bmp, sizeof(BITMAP), 0);
  if GetObject(ctx.Hbmp_Mem, sizeof(BITMAP), @Mem_bmp) = 0 then
  begin
    errcode := GetLastError;
    if errcode <> 0 then
    begin
      errmsg := SysErrorMessage(errcode);
      av_log(s, AV_LOG_ERROR, 'Mem GetObject (error %li: %s)'#10, errcode, PAnsiChar(AnsiString(errmsg)));
      Result := AVERROR_IO;
      Exit;
    end
    else
    begin
      Mem_bmp.bmType := 0;
      Mem_bmp.bmWidth := ctx.width;
      Mem_bmp.bmHeight := ctx.height;
      Mem_bmp.bmWidthBytes := ctx.width * ctx.bpp div 8;
      Mem_bmp.bmPlanes := 1;
      Mem_bmp.bmBitsPixel := ctx.bpp;
      Mem_bmp.bmBits := nil;
      av_log(s, AV_LOG_WARNING,
             'Mem GetObject failed. Force Bitmap type %li, size %lix%lix%i, ' +
             '%i planes of width %li bytes'#10,
             Mem_bmp.bmType, Mem_bmp.bmWidth, Mem_bmp.bmHeight, Mem_bmp.bmBitsPixel,
             Mem_bmp.bmPlanes, Mem_bmp.bmWidthBytes);
    end;
  end
  else
    av_log(s, AV_LOG_DEBUG,
           'Mem Using Bitmap type %li, size %lix%lix%i, ' +
           '%i planes of width %li bytes'#10,
           Mem_bmp.bmType, Mem_bmp.bmWidth, Mem_bmp.bmHeight, Mem_bmp.bmBitsPixel,
           Mem_bmp.bmPlanes, Mem_bmp.bmWidthBytes);
  if SelectObject(ctx.Mem_hdc, ctx.Hbmp_Mem) = 0 then
  begin
    av_log(s, AV_LOG_ERROR, 'Mem SelectObject (error %li)'#10, GetLastError);
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
    av_log(s, AV_LOG_ERROR, 'image depth %i not supported ... aborting'#10, ctx.bpp);
    Result := -1;
    Exit;
  end;
  ctx.size := bmp.bmWidthBytes * bmp.bmHeight * bmp.bmPlanes;

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
    av_log(s, AV_LOG_INFO, 'frame rate assume as %d/%d.'#10, ap.time_base.num, ap.time_base.den);
  end;

//  if (s.show_frame <> 0) and (s.window_handle = 0) then
  if ctx.show_frame <> 0 then
    ctx.frame := TFrameForm.Create(ctx);

  st.codec.codec_type := AVMEDIA_TYPE_VIDEO;
  st.codec.codec_id   := CODEC_ID_RAWVIDEO;
  st.codec.width      := ctx.width;
  st.codec.height     := ctx.height;
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
    av_log(s, AV_LOG_INFO, 'Found window %s, ', title);
  av_log(s, AV_LOG_INFO, 'ready for capturing %ix%ix%i at (%i,%i)'#10,
         ctx.width, ctx.height, ctx.bpp, ctx.x_off, ctx.y_off);

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

  (* Blit screen grab *)
//  if not BitBlt(s.window_hdc, 0, 0, s.width, s.height,
//                s.source_hdc, s.x_off, s.y_off, SRCCOPY) then
//  begin
//    av_log(s1, AV_LOG_ERROR, 'Failed to capture image (error %li)'#10, GetLastError);
//    Result := -1;
//    Exit;
//  end;

  //============================================================
  if not PrintWindow(s.window_handle, s.Mem_hdc, 0) then
  begin
    av_log(s1, AV_LOG_ERROR, 'PrintWindow to Mem_hdc failed (error %li)'#10, GetLastError);
    Result := -1;
    Exit;
  end;
//  frmScreenCapture.img1.Canvas.Lock;
//  BitBlt(frmScreenCapture.img1.Canvas.Handle, 0,0, s.width, s.height, s.Mem_hdc, 0,0, SRCCOPY);
//  frmScreenCapture.img1.Canvas.Unlock;

  if not BitBlt(s.window_hdc, 0, 0, s.width, s.height,
                s.Mem_hdc, s.x_off, s.y_off, SRCCOPY) then
  begin
    av_log(s1, AV_LOG_ERROR, 'Failed to BitBlt image (error %li)'#10, GetLastError);
    Result := -1;
    Exit;
  end;
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

            if s.window_handle <> 0 then
            begin
              if ((s.client <> 0) and GetClientRect(s.window_handle, rect)) or
                ((s.client = 0) and GetWindowRect(s.window_handle, rect)) then
              begin
                if s.client <> 0 then
                begin
                  ClientToScreen(s.window_handle, rect.TopLeft);
                  ClientToScreen(s.window_handle, rect.BottomRight);
                end;
                //av_log(s1, AV_LOG_DEBUG, 'Pos(%li,%li) . (%li,%li)'#10, x, y, x - rect.left, y - rect.top);
                Dec(x, rect.left);
                Dec(y, rect.top);
              end
              else
              begin
                av_log(s1, AV_LOG_ERROR, 'Couldn''t draw icon: %li'#10, GetLastError);
                s.cursor := 0; // do not capture cursor any more
              end;
            end;

            Dec(x, s.x_off);
            Dec(y, s.y_off);
            if not DrawIcon(s.window_hdc, x, y, icon) then
            begin
              av_log(s1, AV_LOG_ERROR, 'Couldn''t draw icon: error %li'#10, GetLastError);
              s.cursor := 0; // do not capture cursor any more
            end;
          end
          else if icon <> 0 then
          begin
            av_log(s1, AV_LOG_ERROR, 'Couldn''t get icon info: error %li'#10, GetLastError);
            s.cursor := 0; // do not capture cursor any more
          end
          else
          begin
            av_log(s1, AV_LOG_ERROR, 'Couldn''t copy icon: error %li'#10, GetLastError);
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
      av_log(s1, AV_LOG_ERROR, 'Couldn''t get cursor info: error %li'#10, GetLastError);
      s.cursor := 0; // do not capture cursor any more
    end;
  end;

  (* Get bits *)
  if GetBitmapBits(s.hbmp, s.size, pkt.data) = 0 then
  begin
    av_log(s1, AV_LOG_ERROR, 'GetBitmapBits failed (error %li)'#10, GetLastError);
    Result := -1;
    Exit;
  end;

  Result := s.size;
end;

function win32grab_read_close(s: PAVFormatContext): Integer; cdecl;
var
  ctx: Pwin32_grab;
begin
  ctx := s.priv_data;

  // release resource
  if ctx.source_hdc <> 0 then
    ReleaseDC(ctx.window_handle, ctx.source_hdc);
  if ctx.window_hdc <> 0 then
    DeleteDC(ctx.window_hdc);
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
