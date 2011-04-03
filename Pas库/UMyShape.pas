{*******************************************************}
{                                                       }
{       TMyShape                                        }
{                                                       }
{       版权所有 (C) 2011 Codeup                        }
{                                                       }
{*******************************************************}

(*
 *  一个可移动伸缩的虚框，可以实现运行设计时。
 *  2011-04-03 修改 增加Active属性，控制显示边框与否。
 *                  在构造函数中添加Parent赋值，外部减少了一次操作。
 *)
unit UMyShape;

interface
uses
  Classes, Controls, ExtCtrls, Graphics, SysUtils, Types;
type
  //事件类型，为TMyShape添加一个OnReSize事件。
  TMyEvent = procedure of object;
  //注意 TMyShape 使用Cursor为识别依据，不可以修改Cursor的值。
  //虚框定位使用VisualTop，VisualLeft，VisualHeight，VisualWidth来定位。Visual和真正的Left什么的差半个DotWidth
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
    Active: Boolean;//控制是否绘制边框，与Visible不同，可相应事件 Add by Codeup 2011-04-03
    //限制边界，一般为父控件的边
    MinVisualLeft,MinVisualTop,MaxVisualWidth,MaxVisualHeight: Integer;
  published
    {Create 函数}
    constructor Create(AOwner: TComponent);override;
    {可视的Left Top Height Width 外部操作都通过这几个参数来获取调整框体的大小位置}
    property  VisualLeft: Integer read ReadVisualLeft write SetVisualLeft;
    property  VisualTop: Integer read ReadVisualTop  write SetVisualTop;
    property  VisualWidth: Integer read ReadVisualWidth  write SetVisualWidth;
    property  VisualHeight: Integer read ReadVisualHeight  write SetVisualHeight;
    {边缘锚点的点宽}
    property  DotWidth: Integer read FDotWidth write SetDotWidth;//DotWidth是偶数比较合适
    {虚框线上点长}
    property  DashLength: Integer read FDashLength write SetDashLength;
    property  ScaleFixed: Boolean read ReadScaleFixed;
    { 事件}
    property  OnReSize: TMyEvent read FOnReSize write FOnReSize;
    property  OnPaint: TMyEvent read FOnPaint write FOnPaint;
    property  OnMove: TMyEvent read FOnMove write FOnMove;
    property  OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property  OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property  OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    {设置长宽固定比例}
    procedure SetScale(ScaleWidth, ScaleHeight: Integer);
    {取消长宽固定比例}
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
  Parent := TWinControl(AOwner);  //Parent赋值放在Create中，外部减少一次赋值操作  Add by Codeup 2011-04-03
end;


{-------------------------------------------------------------------------------
  过程名:    TMyShape.Draw8Dots
  作者:      Administrator
  日期:      2010.11.02
  参数:      无
  返回值:    无
  绘制虚框周围的8个黑点，类似于锚点
-------------------------------------------------------------------------------}
procedure TMyShape.Draw8Dots;
begin
  DrawDot(0,0,1); //左上
  DrawDot((Width div 2),0,2);//上中
  DrawDot(Width,0,3);//右上
  DrawDot(Width,(Height div 2),4);//右中
  DrawDot(Width,Height,5);//右下
  DrawDot((Width div 2),Height,6);//下中
  DrawDot(0,Height,7);//左下
  DrawDot(0,(Height div 2),8);//左中
end;

procedure TMyShape.Draw4Lines;
var
  HalfWidth:Integer;
begin
  HalfWidth := DotWidth div 2;
  DrawDotLine(0 + DotWidth,0 + HalfWidth,Width - DotWidth,0 + HalfWidth);//上
  DrawDotLine(0 + HalfWidth,0 + DotWidth,0 + HalfWidth,Height - DotWidth);//左
  DrawDotLine(0 + DotWidth,Height - HalfWidth,Width - DotWidth,Height - HalfWidth);//下
  DrawDotLine(Width - HalfWidth,0 + DotWidth,Width - HalfWidth,Height - DotWidth);//右
end;

{-------------------------------------------------------------------------------
  过程名:    TMyShape.DrawDot
  作者:      Administrator
  日期:      2010.11.02
  参数:      X, Y, Position: Integer XY为原点坐标，Position是八个点
  返回值:    无
  绘制一个黑点
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
  过程名:    TMyShape.MouseDown
  作者:      Administrator
  日期:      2010.11.02
  参数:      Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  返回值:    无
  鼠标点下，根据鼠标形状设置标志位，供MouseMove使用
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
  过程名:    TMyShape.MouseMove
  作者:      Administrator
  日期:      2010.11.02
  参数:      Shift: TShiftState; X, Y: Integer
  返回值:    无
  核心函数，当鼠标移动时根据XY设置鼠标的形状，同时根据标志位来改变MyShape的形状位置
  每次改变MyShape的值都进行限制检查，如果查出范围则回溯。
-------------------------------------------------------------------------------}
procedure TMyShape.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  temp1,temp2:Integer;//辅助临时变量，为了控制MyShape的活动和大小范围
begin
  inherited;
  //定位鼠标位置，确定鼠标的形状
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

  if sizeMode = smNone then Exit;  //鼠标左键未按下则不处理

  //MyShape移动
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

  //MySpace左变宽
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

  //MyShape右变宽
  if (SizeMode and smRight) <> 0 then
  //temp1:VisualWidth
  begin
    temp1 := ClickVisualWidth + (x - clickx);

    VisualWidth := temp1;
  end;

  //MyShape上变长
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

  //MyShape下变长
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
  过程名:    TMyShape.MouseUp
  作者:      Administrator
  日期:      2010.11.02
  参数:      Button: TMouseButton; Shift: TShiftState; X, Y: Integer
  返回值:    无
  鼠标抬起则设置标志位 smNone
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
  过程名:    TMyShape.Paint
  作者:      Administrator
  日期:      2010.11.02
  参数:      无
  返回值:    无
  绘图时绘制八个锚点和虚线
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


{可视属性读取}
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

{可视属性设置}
procedure TMyShape.SetVisualHeight(const Value: Integer);//VisualHeight 的取值范围在此进行检测
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
  //固定比例 Height联动Width，对 改变进行检测
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
  if Assigned(FOnReSize) then FOnReSize;//触发OnReSize事件
end;
{设置VisualWidth}
procedure TMyShape.SetVisualWidth(const Value: Integer);//VisualWidth 的取值范围在此进行检测
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
  //固定比例 Width联动Height，对 改变进行检测
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
  if Assigned(FOnReSize) then FOnReSize;//触发OnReSize事件
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
  过程名:    TMyShape.SetDotWidth
  作者:      Administrator
  日期:      2010.11.02
  参数:      Value: Integer
  返回值:    无
  设置锚点点宽
-------------------------------------------------------------------------------}
procedure TMyShape.SetDotWidth(Value: Integer);
begin
  FDotWidth:= Value;
end;

{-------------------------------------------------------------------------------
  过程名:    TMyShape.SetDashLength
  作者:      Administrator
  日期:      2010.11.02
  参数:      Value: Integer
  返回值:    无
  设置虚框上点的点长
-------------------------------------------------------------------------------}
procedure TMyShape.SetDashLength(Value: Integer);
begin
  FDashLength := Value;
end;

{-------------------------------------------------------------------------------
  过程名:    TMyShape.DrawDotLine
  作者:      Administrator
  日期:      2010.11.02
  参数:      X1, Y1, X2, Y2:Integer
  返回值:    无
  绘制虚线，黑白相间。从指定点(X1,Y1)到(X2,Y2)。
  目前只支持水平或垂直线。
-------------------------------------------------------------------------------}
procedure TMyShape.DrawDotLine(X1, Y1, X2, Y2:Integer);
begin
  if X1 = X2 then
  begin
    //竖线
    DrawVLine(X1,Y1,Y2);
  end
  else
  begin
    //横线
    DrawHLine(Y1,X1,X2);
  end;
end;

{-------------------------------------------------------------------------------
  过程名:    TMyShape.DrawHLine
  作者:      Administrator
  日期:      2010.11.02
  参数:      Y, X1, X2: Integer
  返回值:    无
  绘制水平虚线
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
  过程名:    TMyShape.DrawVLine
  作者:      Administrator
  日期:      2010.11.02
  参数:      X, Y1, Y2: Integer
  返回值:    无
  绘制垂直虚线
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
  过程名:    TMyShape.InitMyShape
  作者:      Administrator
  日期:      2010.11.03
  参数:      无
  返回值:    无
  Init MyShape 进行初值设定
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
  过程名:    TMyShape.SetScale
  作者:      CodeUp
  日期:      2010.11.03
  参数:      ScaleWidth, ScaleHeight: Integer
  返回值:    无
  设置长宽固定比例，为FScaleWidth 和 FScaleHeight设置，并设FReSizeMode为rsmFixed
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
  过程名:    TMyShape.CancleScale
  作者:      Administrator
  日期:      2010.11.03
  参数:      无
  返回值:    无
  取消长宽按比例大小改变 设FReSizeMode为rsmVariable
-------------------------------------------------------------------------------}
procedure TMyShape.CancleScale;
begin
  FReSizeMode := rsmVariable;
end;

{-------------------------------------------------------------------------------
  过程名:    TMyShape.ReadScaleFixed
  作者:      CodeUp
  日期:      2010.11.03
  参数:      无
  返回值:    Boolean
  读取ScaleFixed状态，实际上是FReSizeMode的类型，如果是rsmFixed则返回True，否则返回False
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
  过程名:    TMyShape.LocateMouse
  作者:      CodeUp
  日期:      2010.11.04
  参数:      X, Y: Integer 当前鼠标位置
  返回值:    Integer
  定位鼠标当前位置，返回值为1到8的数字，分别为从左上角开始顺时针转到左侧的八个锚点
  如果不在锚点则返回0
-------------------------------------------------------------------------------}
function TMyShape.LocateMouse(X, Y: Integer): Integer;
var
  HalfHeight,HalfWidth,HalfPrecision: Integer;
begin
  HalfHeight := Height div 2;
  HalfWidth := Width div 2;
  HalfPrecision := Precision div 2;

  Result := 0;  //不在锚点上
  if X < Precision then
  begin
    if Y < Precision then Result := 1; //左上
    if Y > (Height - Precision) then Result := 7; //左下
    if (Y < (HalfHeight + HalfPrecision)) and (Y > (HalfHeight - HalfPrecision)) then Result := 8; //左中
  end;
  if Y < Precision then
  begin
    if X > (Width - Precision) then Result := 3; // 右上
    if (X < (HalfWidth + HalfPrecision)) and (X > (HalfWidth - HalfPrecision)) then Result := 2; //上中
  end;
  if X > (Width - Precision) then
  begin
    if Y > (Height - Precision) then Result := 5; //右下
    if (Y < (HalfHeight + HalfPrecision)) and (Y > (HalfHeight - HalfPrecision)) then Result := 4; //右中
  end;
  if Y > (Height - Precision) then
  begin
    if (X < (HalfWidth + HalfPrecision)) and (X > (HalfWidth - HalfPrecision)) then Result := 6; //下中
  end;
end;

end.
