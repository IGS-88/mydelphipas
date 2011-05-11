unit uSelector;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TCenterForm = class;
  TSelector = class(TCustomForm)
  private
    FBorderWidth: Integer;
    FullRgn, ClientRgn, CtlRgn: THandle;
    sizeMode: Integer;
    ClickX, ClickY: Integer;

    function NoReSize: Boolean;
    procedure WmNcHitTest(var msg: TWmNcHitTest); message wm_NcHitTest;
    procedure WMSIZING(var Msg: TMessage); message WM_SIZING;
    function LocateMouse(X, Y: Integer): Integer;
    procedure DoInvisible;
  protected
    procedure Resizing(State: TWindowState); override;
  public
    destructor Destroy; override;
    constructor Create; overload;

    procedure SetPosition(LeftTop: TPoint; AWidth, AHeight: Integer);
    procedure SetBorderWidht(NewBorderWidth: Integer);
    function  GetClient: TRect;
  published

  end;

  TCenterForm = class(TCustomForm)
  private

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
  published 

  end;
implementation

{ TBorderForm }
const
  MinWidth = 20;
  MinHeight = 20;

constructor TSelector.Create;
begin
  CreateNew(nil);
  FBorderWidth := 5;
  FormStyle := fsStayOnTop;
  BorderStyle := bsNone;
  Color := clRed;

  DoInvisible;
end;

destructor TSelector.Destroy;
begin

  inherited;
end;

procedure TSelector.DoInvisible;
var
  X, Y: Integer;
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
  SetWindowRgn(Handle, FullRgn, TRUE);
end;

function TSelector.GetClient: TRect;
begin
  Result.Left := Left + FBorderWidth;
  Result.Top := Top + FBorderWidth;
  Result.Right := Left + Width - FBorderWidth;
  Result.Bottom := Top + Height - FBorderWidth;
end;

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

function TSelector.NoReSize: Boolean;
var
  NoResize: Boolean;
begin
  NoResize := False;
  if Width <= 20 then
    NoResize := True;
  if Height <= 20 then
    NoResize := True;

  Result := NoResize;
end;

procedure TSelector.Resizing(State: TWindowState);
begin
  inherited;

  DoInvisible;
end;

procedure TSelector.SetBorderWidht(NewBorderWidth: Integer);
begin
  FBorderWidth := NewBorderWidth;
  DoInvisible;
end;

procedure TSelector.SetPosition(LeftTop: TPoint; AWidth,
  AHeight: Integer);
begin
  Left := LeftTop.X - FBorderWidth;
  Top := LeftTop.Y - FBorderWidth;
  Width := AWidth;
  Height := AHeight;
end;

procedure TSelector.WmNcHitTest(var msg: TWmNcHitTest);
var
  pt: tpoint;
begin
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
                 {
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
  end;            }
end;

{ TCenterForm }

constructor TCenterForm.Create(AOwner: TComponent);
//var
begin
  inherited;
  BorderStyle := bsNone;
  AlphaBlend := True;
  AlphaBlendValue := 100;

//  Left :=
end;

destructor TCenterForm.Destroy;
begin

  inherited;
end;

end.

