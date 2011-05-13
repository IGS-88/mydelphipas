unit uSelector;

interface
uses
  Windows, Messages, SysUtils, StdCtrls, Forms, Classes, Dialogs, Graphics, ExtCtrls, Controls;
type
  TSelector = class(TForm)
  private
    FBorderWidth: Integer;
    FShape: TShape;
    procedure CreateShape;
    function  LocateMouse(APt: TPoint): Integer;
  protected
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetCenterRect: TRect;
  published

  end;
implementation
const
  psCenter = 0;
  psLeft = 1;
  psRight = 2;
  psTop = 4;
  psBottom = 8;

{ TSelector }

constructor TSelector.Create(AOwner: TComponent);
begin
  inherited CreateNew(nil);

  FBorderWidth := 5;
  BorderStyle := bsNone;

  TransparentColor := True;
  TransparentColorValue := 10;
  Color := TransparentColorValue;
  CreateShape;
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
begin
  Result.Left := Left + FBorderWidth;
  Result.Top := Top + FBorderWidth;
  Result.Right := Left + Width - FBorderWidth;
  Result.Bottom := Top + Height - FBorderWidth;
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

  Result := ps;
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

end.

