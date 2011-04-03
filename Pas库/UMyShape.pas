{*******************************************************}
{                                                       }
{       TMyShape                                        }
{                                                       }
{       ��Ȩ���� (C) 2011 Codeup                        }
{                                                       }
{*******************************************************}

(*
 *  һ�����ƶ���������򣬿���ʵ���������ʱ��
 *  2011-04-03 �޸� ����Active���ԣ�������ʾ�߿����
 *                  �ڹ��캯�������Parent��ֵ���ⲿ������һ�β�����
 *)
unit UMyShape;

interface
uses
  Classes, Controls, ExtCtrls, Graphics, SysUtils, Types;
type
  //�¼����ͣ�ΪTMyShape���һ��OnReSize�¼���
  TMyEvent = procedure of object;
  //ע�� TMyShape ʹ��CursorΪʶ�����ݣ��������޸�Cursor��ֵ��
  //���λʹ��VisualTop��VisualLeft��VisualHeight��VisualWidth����λ��Visual��������Leftʲô�Ĳ���DotWidth
  TMyShape = class(TShape)
  private
    { Private declarations }

    ClickX,
    ClickY,
    ClickVisualLeft,
    ClickVisualTop,
    ClickVisualWidth,
    ClickVisualHeight: Integer;
    sizeMode: Integer;
    FOnReSize: TMyEvent;
    FOnPaint: TMyEvent;
    FOnMove: TMyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;

    FReSizeMode: Integer;
    FScaleWidth: Integer;
    FScaleHeight: Integer;
    FDotWidth: Integer;
    FDashLength: Integer;

    procedure InitMyShape();
    function  LocateMouse(X,Y:Integer):Integer;

    procedure DrawDot(X:Integer;Y:Integer;Position:Integer);
    procedure Draw8Dots();
    procedure Draw4Lines();
    procedure DrawDotLine(X1,Y1,X2,Y2:Integer);
    procedure DrawHLine(Y,X1,X2:Integer);
    procedure DrawVLine(X,Y1,Y2:Integer);

    procedure SetVisualLeft(const Value: Integer);
    function  ReadVisualLeft():Integer;
    procedure SetVisualTop(const Value: Integer);
    function  ReadVisualTop():Integer;
    procedure SetVisualWidth(const Value: Integer);
    function  ReadVisualWidth():Integer;
    procedure SetVisualHeight(const Value: Integer);
    function  ReadVisualHeight():Integer;

    procedure SetDotWidth(Value: Integer);
    procedure SetDashLength(Value: Integer);
    function  ReadScaleFixed():Boolean;
  protected
    { Protected declarations }

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure Paint; override;
  public
    { Public declarations }
    Active: Boolean;//�����Ƿ���Ʊ߿���Visible��ͬ������Ӧ�¼� Add by Codeup 2011-04-03
    //���Ʊ߽磬һ��Ϊ���ؼ��ı�
    MinVisualLeft,MinVisualTop,MaxVisualWidth,MaxVisualHeight: Integer;
  published
    {Create ����}
    constructor Create(AOwner: TComponent);override;
    {���ӵ�Left Top Height Width �ⲿ������ͨ���⼸����������ȡ��������Ĵ�Сλ��}
    property  VisualLeft: Integer read ReadVisualLeft write SetVisualLeft;
    property  VisualTop: Integer read ReadVisualTop  write SetVisualTop;
    property  VisualWidth: Integer read ReadVisualWidth  write SetVisualWidth;
    property  VisualHeight: Integer read ReadVisualHeight  write SetVisualHeight;
    {��Եê��ĵ��}
    property  DotWidth: Integer read FDotWidth write SetDotWidth;//DotWidth��ż���ȽϺ���
    {������ϵ㳤}
    property  DashLength: Integer read FDashLength write SetDashLength;
    property  ScaleFixed: Boolean read ReadScaleFixed;
    { �¼�}
    property  OnReSize: TMyEvent read FOnReSize write FOnReSize;
    property  OnPaint: TMyEvent read FOnPaint write FOnPaint;
    property  OnMove: TMyEvent read FOnMove write FOnMove;
    property  OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property  OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property  OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    {���ó���̶�����}
    procedure SetScale(ScaleWidth, ScaleHeight: Integer);
    {ȡ������̶�����}
    procedure CancleScale();
  end;
const
  //MinVisualHeight and MinVisualWidth must bigger than Precision
  MinVisualHeight = 24;
  MinVisualWidth = 24;

  Precision = 10;

  //SizeMode Enumer
  smNone = 0;
  smMove = 1;
  smLeft = 2;
  smRight = 4;
  smUp = 8;
  smDown = 16;

  //ReSizeMode Enumer
  rsmFixed = 0;
  rsmVariable = 1;
implementation

{ TMyShape }

constructor TMyShape.Create(AOwner: TComponent);
begin
  inherited;
  InitMyShape;
  Parent := TWinControl(AOwner);  //Parent��ֵ����Create�У��ⲿ����һ�θ�ֵ����  Add by Codeup 2011-04-03
end;


{-------------------------------------------------------------------------------
  ������:    TMyShape.Draw8Dots
  ����:      Administrator
  ����:      2010.11.02
  ����:      ��
  ����ֵ:    ��
  ���������Χ��8���ڵ㣬������ê��
-------------------------------------------------------------------------------}
procedure TMyShape.Draw8Dots;
begin
  DrawDot(0,0,1); //����
  DrawDot((Width div 2),0,2);//����
  DrawDot(Width,0,3);//����
  DrawDot(Width,(Height div 2),4);//����
  DrawDot(Width,Height,5);//����
  DrawDot((Width div 2),Height,6);//����
  DrawDot(0,Height,7);//����
  DrawDot(0,(Height div 2),8);//����
end;

procedure TMyShape.Draw4Lines;
var
  HalfWidth:Integer;
begin
  HalfWidth := DotWidth div 2;
  DrawDotLine(0 + DotWidth,0 + HalfWidth,Width - DotWidth,0 + HalfWidth);//��
  DrawDotLine(0 + HalfWidth,0 + DotWidth,0 + HalfWidth,Height - DotWidth);//��
  DrawDotLine(0 + DotWidth,Height - HalfWidth,Width - DotWidth,Height - HalfWidth);//��
  DrawDotLine(Width - HalfWidth,0 + DotWidth,Width - HalfWidth,Height - DotWidth);//��
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.DrawDot
  ����:      Administrator
  ����:      2010.11.02
  ����:      X, Y, Position: Integer XYΪԭ�����꣬Position�ǰ˸���
  ����ֵ:    ��
  ����һ���ڵ�
-------------------------------------------------------------------------------}
procedure TMyShape.DrawDot(X, Y, Position: Integer);
var
  tX,tY:Integer;
begin
  case Position of
    1:
    begin
      tX:=X;
      tY:=Y;
    end;
    2:
    begin
      tX:=X - (DotWidth div 2);
      tY:=Y;
    end;
    3:
    begin
      tX:=x - DotWidth;
      tY:=y;
    end;
    4:
    begin
      tX:=x - DotWidth;
      ty:=y - (DotWidth div 2);
    end;
    5:
    begin
      tX:=x - DotWidth;
      ty:=y - DotWidth;
    end;
    6:
    begin
      tX:=x - (DotWidth div 2);
      tY:=y - DotWidth;
    end;
    7:
    begin
      tX:=x;
      tY:=y -DotWidth;
    end;
    8:
    begin
      tX:=x;
      tY:=y - (DotWidth div 2);
    end;
  else
    tX := 0;
    tY := 0;
  end;
  Canvas.Pen.Color:=clWhite;
  Canvas.Pen.Style:=psSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.Rectangle(tX,tY,tX+DotWidth,tY+DotWidth);
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.MouseDown
  ����:      Administrator
  ����:      2010.11.02
  ����:      Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  ����ֵ:    ��
  �����£����������״���ñ�־λ����MouseMoveʹ��
-------------------------------------------------------------------------------}
procedure TMyShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if Shift = [ssLeft] then
  begin
    Clickx := x;
    ClickY := y;
    ClickVisualLeft := VisualLeft;
    ClickVisualTop := VisualTop;
    ClickVisualWidth := VisualWidth;
    ClickVisualHeight := VisualHeight;

    case Cursor of
      crSizeNS:
      begin
        if y < Precision then sizeMode := smUp
        else sizeMode := smDown;
      end;
      crSizeWE:
      begin
        if x < Precision then sizeMode := smLeft
        else sizeMode := smRight;
      end;
      crSizeNESW:
      begin
        if x < Precision then sizeMode := smLeft or smDown
        else sizeMode := smRight or smUp;
      end;
      crSizeNWSE:
      begin
        if x < Precision then sizeMode := smLeft or smUp
        else sizeMode := smRight or smDown;
      end;
      crSizeAll:
      begin
        sizeMode := smMove;
      end;
    else
        sizeMode := smNone;
    end;    
  end;

  if Assigned(FOnMouseDown) then
  begin
    FOnMouseDown(Self, Button, Shift, X, Y);
  end;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.MouseMove
  ����:      Administrator
  ����:      2010.11.02
  ����:      Shift: TShiftState; X, Y: Integer
  ����ֵ:    ��
  ���ĺ�����������ƶ�ʱ����XY����������״��ͬʱ���ݱ�־λ���ı�MyShape����״λ��
  ÿ�θı�MyShape��ֵ���������Ƽ�飬��������Χ����ݡ�
-------------------------------------------------------------------------------}
procedure TMyShape.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  temp1,temp2:Integer;//������ʱ������Ϊ�˿���MyShape�Ļ�ʹ�С��Χ
begin
  inherited;
  //��λ���λ�ã�ȷ��������״
  case LocateMouse(X,Y) of
    1: Cursor := crSizeNWSE;
    2: Cursor := crSizeNS;
    3: Cursor := crSizeNESW;
    4: Cursor := crSizeWE;
    5: Cursor := crSizeNWSE;
    6: Cursor := crSizeNS;
    7: Cursor := crSizeNESW;
    8: Cursor := crSizeWE;
  else Cursor := crSizeAll;
  end;

  if sizeMode = smNone then Exit;  //������δ�����򲻴���

  //MyShape�ƶ�
  if SizeMode = smMove then
  //temp1:VisualLeft ; temp2:VisualTop
  begin
    temp1 := VisualLeft + (x - clickx);
    if temp1 < MinVisualLeft then
    temp1 := MinVisualLeft;
    if (temp1 + ClickVisualWidth) > (MinVisualLeft + MaxVisualWidth) then
    begin
      temp1 := VisualLeft;
    end;
    temp2 := VisualTop + (y - clicky);
    if temp2 < MinVisualTop then
    temp2 := MinVisualTop;
    if (temp2 + ClickVisualHeight) > (MinVisualTop + MaxVisualHeight) then
    begin
      temp2 := VisualTop;
    end;

    VisualLeft := temp1;
    VisualTop := temp2;

    Exit;
  end;

  //MySpace����
  if (SizeMode and smLeft) <> 0 then
  //temp1:VisualLeft ; temp2:VisualWidth
  begin
    temp1 := VisualLeft + (x - clickx);
    if temp1 < MinVisualLeft then
    temp1 := MinVisualLeft;
    temp2 := ClickVisualWidth - (temp1 - clickVisualLeft);
    if temp2 < MinVisualWidth then
    begin
      temp1 := ClickVisualWidth + ClickVisualLeft - MinVisualWidth;
      temp2 := MinVisualWidth
    end;

    VisualLeft := temp1;
    VisualWidth := temp2;
  end;

  //MyShape�ұ��
  if (SizeMode and smRight) <> 0 then
  //temp1:VisualWidth
  begin
    temp1 := ClickVisualWidth + (x - clickx);

    VisualWidth := temp1;
  end;

  //MyShape�ϱ䳤
  if (SizeMode and smUp) <> 0 then
  //temp1:VisualTop ; temp2:VisualHeight
  begin
    temp1 := VisualTop + (y - clicky);
    if temp1 < MinVisualTop then
    temp1 := MinVisualTop;
    temp2 := ClickVisualHeight - (temp1 - clickVisualTop);
    if temp2 < MinVisualHeight then
    begin
      temp1 := ClickVisualHeight + ClickVisualTop - MinVisualHeight;
      temp2 := MinVisualHeight;
    end;

    VisualTop := temp1;
    VisualHeight := temp2;
  end;

  //MyShape�±䳤
  if (SizeMode and smDown) <> 0 then
  //temp1:VisualHeight
  begin
    temp1 := ClickVisualHeight + (y - clicky);

    VisualHeight := temp1;
  end;

  if Assigned(FOnMouseMove) then
  begin
    FOnMouseMove(Self, Shift, X, Y);
  end;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.MouseUp
  ����:      Administrator
  ����:      2010.11.02
  ����:      Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  ����ֵ:    ��
  ���̧�������ñ�־λ smNone
-------------------------------------------------------------------------------}
procedure TMyShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  sizeMode := smNone;

  if Assigned(FOnMouseUp) then
  begin
    FOnMouseUp(Self, Button, Shift, X, Y);
  end;
end;


{-------------------------------------------------------------------------------
  ������:    TMyShape.Paint
  ����:      Administrator
  ����:      2010.11.02
  ����:      ��
  ����ֵ:    ��
  ��ͼʱ���ư˸�ê�������
-------------------------------------------------------------------------------}
procedure TMyShape.Paint;
begin
  inherited;
  if not Active then
    Exit;
  Draw4Lines;
  Draw8Dots;
  if Assigned(FOnPaint) then
    FOnPaint;
end;


{�������Զ�ȡ}
function TMyShape.ReadVisualHeight: Integer;
begin
  Result := Height - DotWidth;
end;
function TMyShape.ReadVisualLeft: Integer;
begin
  Result := Left + (DotWidth div 2);
end;
function TMyShape.ReadVisualTop: Integer;
begin
  Result := Top + (DotWidth div 2);
end;
function TMyShape.ReadVisualWidth: Integer;
begin
  Result := Width - DotWidth;
end;

{������������}
procedure TMyShape.SetVisualHeight(const Value: Integer);//VisualHeight ��ȡֵ��Χ�ڴ˽��м��
var
  temp1: Integer;//VisualHeight
  temp2: Integer;//VisualWidth
begin
  temp1 := Value;
  if (VisualTop + temp1) > (MinVisualTop + MaxVisualHeight) then
  temp1 := VisualHeight;
  if temp1 < MinVisualHeight then
  temp1 := MinVisualHeight;

  if FReSizeMode = rsmFixed then
  //�̶����� Height����Width���� �ı���м��
  begin
    temp2 := temp1 * FScaleHeight div FScaleWidth;
    if ((VisualLeft + temp2) > (MinVisualLeft + MaxVisualWidth)) or (temp2 < MinVisualWidth) then
    begin
      temp1 := VisualHeight;
    end
    else
    begin
      Width := temp2 + DotWidth;
    end;
  end;

  Height := temp1 + DotWidth;
  if Assigned(FOnReSize) then FOnReSize;//����OnReSize�¼�
end;
{����VisualWidth}
procedure TMyShape.SetVisualWidth(const Value: Integer);//VisualWidth ��ȡֵ��Χ�ڴ˽��м��
var
  temp1: Integer;//VisualWidth
  temp2: Integer;//VisualHeight
begin
  temp1 := Value;
  if (VisualLeft + temp1) > (MinVisualLeft + MaxVisualWidth) then
  temp1 := VisualWidth;
  if temp1 < MinVisualWidth then
  temp1 := MinVisualWidth;

  if FReSizeMode = rsmFixed then
  //�̶����� Width����Height���� �ı���м��
  begin
    temp2 := temp1 * FScaleWidth div FScaleHeight;
    if ((VisualTop + temp2) > (MinVisualTop + MaxVisualHeight)) or (temp2 < MinVisualHeight) then
    begin
      temp1 := VisualWidth;
    end
    else
    begin
      Height := temp2 + DotWidth;
    end;
  end;

  Width := temp1 + DotWidth;
  if Assigned(FOnReSize) then FOnReSize;//����OnReSize�¼�
end;
procedure TMyShape.SetVisualLeft(const Value: Integer);
begin
  Left := Value - (DotWidth div 2);
  if Assigned(FOnMove) then
    FOnMove;
end;
procedure TMyShape.SetVisualTop(const Value: Integer);
begin
  Top := Value - (DotWidth div 2);
  if Assigned(FOnMove) then
    FOnMove;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.SetDotWidth
  ����:      Administrator
  ����:      2010.11.02
  ����:      Value: Integer
  ����ֵ:    ��
  ����ê����
-------------------------------------------------------------------------------}
procedure TMyShape.SetDotWidth(Value: Integer);
begin
  FDotWidth:= Value;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.SetDashLength
  ����:      Administrator
  ����:      2010.11.02
  ����:      Value: Integer
  ����ֵ:    ��
  ��������ϵ�ĵ㳤
-------------------------------------------------------------------------------}
procedure TMyShape.SetDashLength(Value: Integer);
begin
  FDashLength := Value;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.DrawDotLine
  ����:      Administrator
  ����:      2010.11.02
  ����:      X1, Y1, X2, Y2:Integer
  ����ֵ:    ��
  �������ߣ��ڰ���䡣��ָ����(X1,Y1)��(X2,Y2)��
  Ŀǰֻ֧��ˮƽ��ֱ�ߡ�
-------------------------------------------------------------------------------}
procedure TMyShape.DrawDotLine(X1, Y1, X2, Y2:Integer);
begin
  if X1 = X2 then
  begin
    //����
    DrawVLine(X1,Y1,Y2);
  end
  else
  begin
    //����
    DrawHLine(Y1,X1,X2);
  end;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.DrawHLine
  ����:      Administrator
  ����:      2010.11.02
  ����:      Y, X1, X2: Integer
  ����ֵ:    ��
  ����ˮƽ����
-------------------------------------------------------------------------------}
procedure TMyShape.DrawHLine(Y, X1, X2: Integer);
var
  startX,endX: Integer;
begin
  if X1 > X2 then
  begin
    startX := X2;
    endX := X1;
  end
  else
  begin
    startX := X1;
    endX := X2;
  end;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psSolid;
  while startX < endX do
  begin
    if Canvas.Pen.Color = clBlack then
      Canvas.Pen.Color := clWhite
    else
      Canvas.Pen.Color := clBlack;

    Canvas.MoveTo(startX , Y);
    Canvas.LineTo(startX + FDashLength , Y);
    startX := startX + FDashLength;
  end;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.DrawVLine
  ����:      Administrator
  ����:      2010.11.02
  ����:      X, Y1, Y2: Integer
  ����ֵ:    ��
  ���ƴ�ֱ����
-------------------------------------------------------------------------------}
procedure TMyShape.DrawVLine(X, Y1, Y2: Integer);
var
  startY,endY: Integer;
begin
  if Y1 > Y2 then
  begin
    startY := Y2;
    endY := Y1;
  end
  else
  begin
    startY := Y1;
    endY := Y2;
  end;
  Canvas.Pen.Color := clBlack;
  while startY < endY do
  begin
    if Canvas.Pen.Color = clBlack then
      Canvas.Pen.Color := clWhite
    else
      Canvas.Pen.Color := clBlack;

    Canvas.MoveTo(X , startY);
    Canvas.LineTo(X , startY + FDashLength);
    startY := startY + FDashLength;
  end;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.InitMyShape
  ����:      Administrator
  ����:      2010.11.03
  ����:      ��
  ����ֵ:    ��
  Init MyShape ���г�ֵ�趨
-------------------------------------------------------------------------------}
procedure TMyShape.InitMyShape;
begin
  Brush.Style := bsClear;
  Pen.Style := psClear;

  DotWidth := 8;
  DashLength := 3;

  sizeMode := smNone;

  FReSizeMode := rsmVariable;
  FScaleWidth := 1;
  FScaleHeight := 1;

  Active := True;
  MinVisualLeft := 0;
  MinVisualTop := 0;
  MaxVisualWidth := High(Integer);
  MaxVisualHeight := High(Integer);
  VisualLeft := 0;
  VisualTop := 0;

  OnReSize := nil;
  FOnMouseDown := nil;
  FOnMouseUp := nil;
  FOnMouseMove := nil;
  FOnReSize := nil;
  FOnPaint := nil;
  FOnMove := nil;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.SetScale
  ����:      CodeUp
  ����:      2010.11.03
  ����:      ScaleWidth, ScaleHeight: Integer
  ����ֵ:    ��
  ���ó���̶�������ΪFScaleWidth �� FScaleHeight���ã�����FReSizeModeΪrsmFixed
-------------------------------------------------------------------------------}
procedure TMyShape.SetScale(ScaleWidth, ScaleHeight: Integer);
var
  m, n, r : Integer;
begin
  if ScaleWidth > ScaleHeight then
  begin
    m := ScaleWidth;
    n := ScaleHeight;
  end
  else
  begin
    m := ScaleHeight;
    n := ScaleWidth;
  end;
  r := m mod n;
  while r<>0 do
  begin
    m := n;
    n := r;
    r := m mod n;
  end;
  FScaleWidth := ScaleWidth div n;
  FScaleHeight := ScaleHeight div n;
  FReSizeMode := rsmFixed;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.CancleScale
  ����:      Administrator
  ����:      2010.11.03
  ����:      ��
  ����ֵ:    ��
  ȡ������������С�ı� ��FReSizeModeΪrsmVariable
-------------------------------------------------------------------------------}
procedure TMyShape.CancleScale;
begin
  FReSizeMode := rsmVariable;
end;

{-------------------------------------------------------------------------------
  ������:    TMyShape.ReadScaleFixed
  ����:      CodeUp
  ����:      2010.11.03
  ����:      ��
  ����ֵ:    Boolean
  ��ȡScaleFixed״̬��ʵ������FReSizeMode�����ͣ������rsmFixed�򷵻�True�����򷵻�False
-------------------------------------------------------------------------------}
function TMyShape.ReadScaleFixed: Boolean;
begin
  if FReSizeMode = rsmFixed then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;  
end;


{-------------------------------------------------------------------------------
  ������:    TMyShape.LocateMouse
  ����:      CodeUp
  ����:      2010.11.04
  ����:      X, Y: Integer ��ǰ���λ��
  ����ֵ:    Integer
  ��λ��굱ǰλ�ã�����ֵΪ1��8�����֣��ֱ�Ϊ�����Ͻǿ�ʼ˳ʱ��ת�����İ˸�ê��
  �������ê���򷵻�0
-------------------------------------------------------------------------------}
function TMyShape.LocateMouse(X, Y: Integer): Integer;
var
  HalfHeight,HalfWidth,HalfPrecision: Integer;
begin
  HalfHeight := Height div 2;
  HalfWidth := Width div 2;
  HalfPrecision := Precision div 2;

  Result := 0;  //����ê����
  if X < Precision then
  begin
    if Y < Precision then Result := 1; //����
    if Y > (Height - Precision) then Result := 7; //����
    if (Y < (HalfHeight + HalfPrecision)) and (Y > (HalfHeight - HalfPrecision)) then Result := 8; //����
  end;
  if Y < Precision then
  begin
    if X > (Width - Precision) then Result := 3; // ����
    if (X < (HalfWidth + HalfPrecision)) and (X > (HalfWidth - HalfPrecision)) then Result := 2; //����
  end;
  if X > (Width - Precision) then
  begin
    if Y > (Height - Precision) then Result := 5; //����
    if (Y < (HalfHeight + HalfPrecision)) and (Y > (HalfHeight - HalfPrecision)) then Result := 4; //����
  end;
  if Y > (Height - Precision) then
  begin
    if (X < (HalfWidth + HalfPrecision)) and (X > (HalfWidth - HalfPrecision)) then Result := 6; //����
  end;
end;

end.
