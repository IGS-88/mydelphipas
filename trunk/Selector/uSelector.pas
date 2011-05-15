unit uSelector;

interface
uses
  Windows, Messages, SysUtils, StdCtrls, Forms, Classes, Dialogs, Graphics, ExtCtrls, Controls;
type
  TSelector = class(TForm)
  private
    FBorderWidth: Integer;
    FShape: TShape;
    FLimitHandle: HWND;
    FMinWidth, FMinHeight, FMaxWidth, FMaxHeight: Integer;
    FMousePos: Integer;
    procedure CreateShape;
    function  LocateMouse(APt: TPoint): Integer;
  protected
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMPosChange(var Message: TWMWINDOWPOSCHANGING);message WM_WINDOWPOSCHANGING;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetCenterRect: TRect;
    procedure SetLimitHWND(handle: HWND);
    procedure SetLimitSize(MinSize, MaxSize: TPoint);
  published

  end;
implementation
const

//Position of mouse
  psCenter = 0;
  psLeft = 1;
  psRight = 2;
  psTop = 4;
  psBottom = 8;
  psNone = 16;
{ TSelector }

constructor TSelector.Create(AOwner: TComponent);
begin
  inherited CreateNew(nil);
  FLimitHandle := GetDesktopWindow;
  FMinWidth := 20;
  FMinHeight := 20;
  FMaxWidth := Screen.Width;
  FMaxHeight := Screen.Height;
  FBorderWidth := 5;
  FMousePos := psNone;
  BorderStyle := bsNone;

  // do not show in taskbar
  SetWindowLong(Handle, GWL_EXSTYLE,
    GetWindowLong(Handle, GWL_EXSTYLE) or
    WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
  // stay on the top
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOOWNERZORDER);

  TransparentColor := True;
  TransparentColorValue := 10;
  Color := TransparentColorValue;
  CreateShape;
end;

procedure TSelector.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := GetDesktopWindow;
end;

procedure TSelector.CreateShape;
begin
  FShape := TShape.Create(Self);
  FShape.Parent := Self;
  FShape.Brush.Color := TransparentColorValue;
  FShape.Pen.Width := FBorderWidth;
  FShape.Pen.Color := clRed;
  FShape.Pen.Style := psSolid;
  FShape.Align := alClient;
  FShape.Show;
end;

destructor TSelector.Destroy;
begin

  inherited;
end;

function TSelector.GetCenterRect: TRect;
var
  lt: TPoint;
  rb: TPoint;
begin
  lt.X := Left + FBorderWidth;
  lt.Y := Top + FBorderWidth;
  rb.X := Left + Width - FBorderWidth;
  rb.Y := Top + Height - FBorderWidth;
  Windows.ScreenToClient(FLimitHandle, lt);
  Windows.ScreenToClient(FLimitHandle, rb);
  Result.TopLeft := lt;
  Result.BottomRight := rb;
end;

function TSelector.LocateMouse(APt: TPoint): Integer;
var
  pt: TPoint;
  ps: Integer;
begin
  pt := ScreenToClient(APt);
  ps := psCenter;
  if pt.X < FBorderWidth then
    ps := ps or psLeft;
  if pt.X > ClientWidth - FBorderWidth then
    ps := ps or psRight;
  if pt.Y < FBorderWidth then
    ps := ps or psTop;
  if pt.Y > ClientHeight - FBorderWidth then
    ps := ps or psBottom;
  FMousePos := ps;
  Result := ps;
end;

procedure TSelector.SetLimitHWND(handle: HWND);
begin
  FLimitHandle := handle;
end;

procedure TSelector.SetLimitSize(MinSize, MaxSize: TPoint);
begin
  FMinWidth := MinSize.X;
  FMinHeight := MinSize.Y;
  FMaxWidth := MaxSize.X;
  FMaxHeight := MaxSize.Y;
end;

procedure TSelector.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  message.MinMaxInfo.ptMinTrackSize := Point(FMinWidth, FMinHeight);
end;

procedure TSelector.WMNCHitTest(var Message: TWMNCHitTest);
begin
  case LocateMouse(Point(Message.XPos, Message.YPos)) of
    psCenter: Message.Result := HTCAPTION;
    psBottom: Message.Result := HTBOTTOM;
    psTop: Message.Result := HTTOP;
    psLeft: Message.Result := HTLEFT;
    psRight: Message.Result := HTRIGHT;
    (psLeft or psTop): Message.Result := HTTOPLEFT;
    (psLeft or psBottom): Message.Result := HTBOTTOMLEFT;
    (psRight or psTop): Message.Result := HTTOPRIGHT;
    (psRight or psBottom): Message.Result := HTBOTTOMRIGHT;
  else
    inherited;
  end;    
end;

procedure TSelector.WMPosChange(var Message: TWMWINDOWPOSCHANGING);
var
  r: TRect;
begin
  Windows.GetWindowRect(FLimitHandle, r);
//  Windows.ClientToScreen(FLimitHandle, r.TopLeft);
//  Windows.ClientToScreen(FLimitHandle, r.BottomRight);
  with message do
  begin
    if WindowPos.x <= r.Left then
    begin
      WindowPos.x := r.Left;
      if (FMousePos and psLeft) <> 0 then
      begin
        WindowPos.flags := windowPos.flags or SWP_NOSIZE;
      end;
    end;

    if WindowPos.y <= r.Top then
    begin
      WindowPos.y := r.Top;
      if (FMousePos and psTop) <> 0 then
      begin
        WindowPos.flags := windowPos.flags or SWP_NOSIZE;
      end;
    end;

    if WindowPos.x + WindowPos.cx >= r.Right then
    begin
      if FMousePos = psCenter then
      begin
        WindowPos.x := r.Right - WindowPos.cx;
        WindowPos.flags := windowPos.flags or SWP_NOSIZE;
      end;

      WindowPos.cx := r.Right - WindowPos.x;
      if WindowPos.cx <= FMinWidth then
      begin
        WindowPos.x := r.Right - FMinWidth;
        WindowPos.cx := FMinWidth;
      end;
    end;

    if WindowPos.y + WindowPos.cy >= r.Bottom then
    begin
      if FMousePos = psCenter then
      begin
        WindowPos.y := r.Bottom - WindowPos.cy;
        WindowPos.flags := windowPos.flags or SWP_NOSIZE;
      end;

      WindowPos.cy := r.Bottom - WindowPos.y;
      if WindowPos.cy <= FMinHeight then
      begin
        WindowPos.y := r.Bottom - FMinHeight;
        WindowPos.cy := FMinHeight;
      end;
    end;

    if WindowPos.cx < FMinWidth then
    begin
      WindowPos.cx := FMinWidth;
    end;

    if WindowPos.cy < FMinHeight then
    begin
      WindowPos.cy := FMinHeight;
    end;
  end;
end;

end.

