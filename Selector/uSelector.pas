unit uSelector;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TCenterForm = class;
  PSelector = ^TSelector;
  //主体窗体，选择框
  TSelector = class(TCustomForm)
  private
    FCenter: TCenterForm;
    FBorderWidth: Integer;
    FullRgn, ClientRgn, CtlRgn: THandle;
    sizeMode: Integer;
    ClickX, ClickY: Integer;

    procedure WmNcHitTest(var msg: TWmNcHitTest); message wm_NcHitTest;
    procedure WMSIZING(var Msg: TMessage); message WM_SIZING;
    function LocateMouse(X, Y: Integer): Integer;
    procedure DoInvisible;
    procedure StayOnTop;
  protected
    procedure Resizing(State: TWindowState); override;
  public
    Moving: Boolean;
    destructor Destroy; override;
    constructor Create; overload;

    procedure SetPosition(LeftTop: TPoint; AWidth, AHeight: Integer); overload;
    procedure SetPosition(LeftTop: TPoint); overload;
    procedure SetClientPos(LeftTop: TPoint; AWidth, AHeight: Integer); overload;
    procedure SetClientPos(LeftTop: TPoint); overload;
    procedure SetBorderWidht(NewBorderWidth: Integer);
    function GetClient: TRect;
  published

  end;

  //中心的透明窗体，负责移动信息传递
  TCenterForm = class(TCustomForm)
  private
    clickX, clickY: Integer;
    clickDown: Boolean;
    FSelector: PSelector;
  protected
    procedure Resizing(State: TWindowState); override;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMMoving(var Message: TWMMoving); message WM_MOVING;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
                                          
  public
    ParentHandle: HWND;
    constructor Create(AOwner: PSelector); overload;
    destructor Destroy; override;
    procedure SetPosition(r: TRect);
  published

  end;
implementation
{ TBorderForm }
const
  MinWidth = 20;
  MinHeight = 20;

constructor TSelector.Create;
begin
  inherited CreateNew(nil);
  Application.NormalizeTopMosts;
  DoubleBuffered := True;
  FBorderWidth := 5;
  BorderStyle := bsNone;
  Color := clRed;

  FCenter := TCenterForm.Create(@Self);
  FCenter.ParentHandle := Handle;
  FCenter.Show;
  StayOnTop;
  DoInvisible;
end;

destructor TSelector.Destroy;
begin
  DeleteObject(ClientRgn);
  DeleteObject(FullRgn);
  DeleteObject(CtlRgn);
  inherited;
end;

//中心透明

procedure TSelector.DoInvisible;
var
  AControl: TControl;
  A, X, Y, CtlX, CtlY: Integer;
begin
  //First,   get   form   region
  FullRgn := CreateRectRgn(0, 0, Width, Height);
  //Find   client   area   region
  X := FBorderWidth;
  Y := FBorderWidth;
  ClientRgn := CreateRectRgn(X, Y, Width - FBorderWidth, Height - FBorderWidth);
  // 'Mask '   out   all   but   non-client   areas
  CombineRgn(FullRgn, FullRgn, ClientRgn, RGN_DIFF);
  //When   the   region   is   all   ready,   put   it   into   effect:
  //Now,   walk   through   all   the   controls   on   the   form   and   'OR '   them
    //   into   the   existing   Full   region.
  for A := 0 to ControlCount - 1 do
  begin
    AControl := Controls[A];
    if (AControl is TWinControl) or (AControl is TGraphicControl) then
      with AControl do
      begin
        if Visible then
        begin
          CtlX := X + Left;
          CtlY := Y + Top;
          CtlRgn := CreateRectRgn(CtlX, CtlY, CtlX + Width, CtlY + Height);
          CombineRgn(FullRgn, FullRgn, CtlRgn, RGN_OR);
        end;
      end;
  end;
  SetWindowRgn(Handle, FullRgn, TRUE);
end;

function TSelector.GetClient: TRect;
begin
  Result.Left := Left + FBorderWidth;
  Result.Top := Top + FBorderWidth;
  Result.Right := Left + Width - FBorderWidth;
  Result.Bottom := Top + Height - FBorderWidth;
end;

//检测鼠标位置

function TSelector.LocateMouse(X, Y: Integer): Integer;
var
  HalfHeight, HalfWidth, HalfPrecision: Integer;
begin
  HalfHeight := Height div 2;
  HalfWidth := Width div 2;
  HalfPrecision := FBorderWidth div 2;

  Result := 0; //不在锚点上
  if X < FBorderWidth then
  begin
    if Y < FBorderWidth then
      Result := 1; //左上
    if Y > (Height - FBorderWidth) then
      Result := 7; //左下
    //    if (Y < (HalfHeight + HalfPrecision)) and (Y > (HalfHeight - HalfPrecision)) then Result := 8; //左中
    if (Y <= (Height - FBorderWidth)) and (Y >= FBorderWidth) then
      Result := 8;
  end;
  if Y < FBorderWidth then
  begin
    if X > (Width - FBorderWidth) then
      Result := 3; // 右上
    //    if (X < (HalfWidth + HalfPrecision)) and (X > (HalfWidth - HalfPrecision)) then Result := 2; //上中
    if (X <= (Width - FBorderWidth)) and (X >= FBorderWidth) then
      Result := 2; //上中
  end;
  if X >= (Width - FBorderWidth) then
  begin
    if Y > (Height - FBorderWidth) then
      Result := 5; //右下
    //    if (Y < (HalfHeight + HalfPrecision)) and (Y > (HalfHeight - HalfPrecision)) then Result := 4; //右中
    if (Y < (Height - FBorderWidth)) and (Y > FBorderWidth) then
      Result := 4; //右中
  end;
  if Y >= (Height - FBorderWidth) then
  begin
    //    if (X < (HalfWidth + HalfPrecision)) and (X > (HalfWidth - HalfPrecision)) then Result := 6; //下中
    if (X <= (Width - FBorderWidth)) and (X >= FBorderWidth) then
      Result := 6;
  end;
end;

//改变大小后重新设置透明

procedure TSelector.Resizing(State: TWindowState);
begin
  inherited;

  DoInvisible;
  if not Moving then
  FCenter.SetPosition(GetClient);
end;

//设定Selector的边框宽度

procedure TSelector.SetBorderWidht(NewBorderWidth: Integer);
begin
  FBorderWidth := NewBorderWidth;
  DoInvisible;
end;

//设定Selector的位置形状

procedure TSelector.SetPosition(LeftTop: TPoint; AWidth,
  AHeight: Integer);
begin
  SetBounds(LeftTop.X, LeftTop.Y, AWidth, AHeight);
end;
//设定Selector的位置

procedure TSelector.SetPosition(LeftTop: TPoint);
begin
  Left := LeftTop.X;
  Top := LeftTop.Y;
end;
//设定Selector的内部位置形状

procedure TSelector.SetClientPos(LeftTop: TPoint; AWidth,
  AHeight: Integer);
begin
  SetBounds(LeftTop.X - FBorderWidth, LeftTop.Y - FBorderWidth, AWidth +
    FBorderWidth * 2, AHeight + FBorderWidth * 2);
end;
//设定Selector的内部位置

procedure TSelector.SetClientPos(LeftTop: TPoint);
begin
  Left := LeftTop.X - FBorderWidth;
  Top := LeftTop.Y - FBorderWidth;
end;

//设置窗口置前

procedure TSelector.StayOnTop;
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  SetWindowPos(FCenter.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
    SWP_NOSIZE);
end;

//相应拖拽伸缩

procedure TSelector.WmNcHitTest(var msg: TWmNcHitTest);
var
  pt: tpoint;
begin
  if msg.Unused = 12 then
  begin
    msg.Result := HTCAPTION;
    //    inherited;
    Exit;
  end;
  pt := point(msg.xpos, msg.ypos);
  pt := ScreentoClient(pt);
  case LocateMouse(pt.X, pt.Y) of
    1: msg.Result := HTTOPLEFT;
    2: msg.Result := HTTOP;
    3: msg.Result := HTTOPRIGHT;
    4: msg.Result := HTRIGHT;
    5: msg.Result := HTBOTTOMRIGHT;
    6: msg.Result := HTBOTTOM;
    7: msg.Result := HTBOTTOMLEFT;
    8: msg.Result := HTLEFT;
  else
    inherited;
  end;
end;

procedure TSelector.WMSIZING(var Msg: TMessage);
begin
  Msg.Result := 1;

  //限制最小高度
  if (PRect(Msg.LPARAM)^.Bottom - PRect(Msg.LPARAM)^.Top) < MinHeight then
  begin
    if PRect(Msg.LPARAM)^.Top = Top then
      PRect(Msg.LPARAM)^.Bottom := PRect(Msg.LPARAM)^.Top + MinHeight
    else
      PRect(Msg.LPARAM)^.Top := PRect(Msg.LPARAM)^.Bottom - MinHeight;
  end;

  //限制最小宽度
  if (PRect(Msg.LPARAM)^.Right - PRect(Msg.LPARAM)^.Left) < MinWidth then
  begin
    if PRect(Msg.LPARAM)^.Left = Left then
      PRect(Msg.LPARAM)^.Right := PRect(Msg.LPARAM)^.Left + MinWidth
    else
      PRect(Msg.LPARAM)^.Left := PRect(Msg.LPARAM)^.Right - MinWidth;
  end;
end;

{ TCenterForm }

constructor TCenterForm.Create(AOwner: PSelector);
var
  r: TRect;
begin
  inherited CreateNew(nil);
  DoubleBuffered := True;
  BorderStyle := bsNone;
  AlphaBlend := True;
  AlphaBlendValue := 1;
  FSelector := AOwner;
  r := FSelector.GetClient;
  SetPosition(r);
end;

destructor TCenterForm.Destroy;
begin

  inherited;
end;

procedure TCenterForm.Resizing(State: TWindowState);
begin
  inherited;
  Cursor := crSizeAll;
end;

procedure TCenterForm.SetPosition(r: TRect);
begin
  SetBounds(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);
end;

procedure TCenterForm.WMMove(var Message: TWMMove);
begin
  FSelector.Moving := False;
end;

procedure TCenterForm.WMMoving(var Message: TWMMoving);
begin
  FSelector.Moving := False;
  FSelector.SetClientPos(Point(Left, Top));
end;

procedure TCenterForm.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTCAPTION;
end;

end.

