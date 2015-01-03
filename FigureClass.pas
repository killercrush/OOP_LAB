unit FigureClass;

interface

uses Graphics, SysUtils;

type
  PPoint = ^TPoint;
  PCircle = ^TCircle;

  TPoint = class
  private { приватные поля и методы }
    X: integer; { координата X }
    Y: integer; { координата Y }
    Color: integer; { цвет }
    Size: integer; { размер }
    Angle: integer; { угол }
    Canvas: TCanvas; // на нем рисуем
    FSelected: boolean;
    procedure SetSelection(Value: boolean); virtual;
  public
    procedure Draw; virtual; { показать }
    procedure Hide; virtual; { стереть }
    constructor Create(aX, aY, aColor: integer; ACanvas: TCanvas); virtual;
    { создать }
    destructor Destroy; override; { удалить }
    procedure Shift(dX, dY: integer); { подвинуть }
    procedure ChangeSize(AScale: double); virtual; abstract; { изменить размер }
    procedure Rotate(AAngle: integer); virtual; abstract; { повернуть }
    function PointInFugure(aX, aY: integer): boolean; virtual;
    property Selected: boolean read FSelected write SetSelection;
  end;

  TCircle = class(TPoint)
  public
    procedure Draw; override;
    procedure Hide; override;
    procedure ChangeSize(AScale: double);
    constructor Create(aX, aY, aColor, aSize: integer; ACanvas: TCanvas);
    destructor Destroy; override;
    function PointInFugure(aX, aY: integer): boolean; override;
    procedure SetSelection(Value: boolean); override;
  end;

  TSquare = class(TPoint)
  public
    procedure Draw; override;
    procedure Hide; override;
    procedure ChangeSize(AScale: double);
    constructor Create(aX, aY, aColor, aSize: integer; ACanvas: TCanvas);
    destructor Destroy; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  PListItem = ^ListItem;

  ListItem = record
    Item: TPoint;
    NextItem: PListItem;
  end;

  TList = class
  public
    FirstItem: PListItem;
    constructor Create(AFirstItem: PListItem = nil);
    destructor Destroy; override;
    procedure AddItem(AItem: TPoint);
  end;

implementation

constructor TPoint.Create(aX, aY, aColor: integer; ACanvas: TCanvas);
begin
  { инициализируем поля экземпляра класса }
  X := aX;
  Y := aY;
  Canvas := ACanvas;
  Color := aColor;
  Selected := true;
  Draw;
end;

destructor TPoint.Destroy;
begin
  { сотрем с экрана класс }
  Hide;
  inherited;
end;

procedure TPoint.Hide;
begin
  { нарисуем класс с 0-м цветом (стираем) }
  Color := $FFFFFF;
  Draw;
end;

function TPoint.PointInFugure(aX, aY: integer): boolean;
begin
  result := (aX = X) and (aY = Y);
end;

procedure TPoint.SetSelection(Value: boolean);
begin
  if Value = FSelected then
    exit;
  FSelected := Value;
  if FSelected then
    Canvas.Pen.Color := $00FF00
  else
    Canvas.Pen.Color := $FFFFFF;
  Canvas.Rectangle(X - 1, Y - 1, X + 2, Y + 2);
end;

procedure TPoint.Shift(dX, dY: integer);
begin
  Hide; { сотрем со старыми координатами }
  X := X + dX;
  Y := Y + dY;
  Draw; { нарисуем с новыми координатами }
end;

procedure TPoint.Draw;
begin
  { рисуем класс }
  Canvas.Pixels[X, Y] := Color;
end;

{ TCircle }

procedure TCircle.ChangeSize(AScale: double);
begin
  Hide;
  Size := Round(Size * AScale);
  Draw;
end;

constructor TCircle.Create(aX, aY, aColor, aSize: integer; ACanvas: TCanvas);
begin
  Size := aSize;
  inherited Create(aX, aY, aColor, ACanvas);
  //Selected := true;
  //Draw;
end;

destructor TCircle.Destroy;
begin
  inherited;
end;

procedure TCircle.Draw;
begin

  Canvas.Pen.Color := Color;
  Canvas.Ellipse(X - Size, Y - Size, X + Size, Y + Size);
end;

procedure TCircle.Hide;
begin
  inherited;
end;

function TCircle.PointInFugure(aX, aY: integer): boolean;
begin
  result := (((aX - X) * (aX - X) + (aY - Y) * (aY - Y)) <= Size * Size);
end;

procedure TCircle.SetSelection(Value: boolean);
begin
  if Value = FSelected then
    exit;
  FSelected := Value;
  if FSelected then
    Canvas.Pen.Color := $00FF00
  else
    Canvas.Pen.Color := $FFFFFF;
  Canvas.Ellipse(X - Size - 1, Y - Size - 1, X + Size + 1, Y + Size + 1);
  Canvas.Ellipse(X - Size + 1, Y - Size + 1, X + Size - 1, Y + Size - 1);
end;

{ TList }

procedure TList.AddItem(AItem: TPoint);
var
  ListItem, Tmp: PListItem;
begin
  New(ListItem);
  ListItem.Item := AItem;
  ListItem.NextItem := nil;

  if FirstItem = nil then
    FirstItem := ListItem
  else
  begin
    Tmp := FirstItem;
    while Tmp.NextItem <> nil do
      Tmp := Tmp.NextItem;
    Tmp.NextItem := ListItem;
  end;
end;

constructor TList.Create(AFirstItem: PListItem);
begin
  FirstItem := AFirstItem;
end;

destructor TList.Destroy;
var
  ListItem, NextItem: PListItem;
begin
  if FirstItem <> nil then
  begin
    ListItem := FirstItem;
    repeat
      NextItem := ListItem.NextItem;
      FreeAndNil(ListItem.Item);
      Dispose(ListItem);
      ListItem := NextItem;
    until ListItem = nil;
  end;
  inherited;
end;

{ TSquare }

procedure TSquare.ChangeSize(AScale: double);
begin
  Hide;
  Size := Round(Size * AScale);
  Draw;
end;

constructor TSquare.Create(aX, aY, aColor, aSize: integer; ACanvas: TCanvas);
begin
  inherited Create(aX, aY, aColor, ACanvas);
  Size := aSize;
end;

destructor TSquare.Destroy;
begin

  inherited;
end;

procedure TSquare.Draw;
begin
  // inherited;
  Canvas.Pen.Color := Color;
  Canvas.Rectangle(X - Size, Y - Size, X + Size, Y + Size);
end;

procedure TSquare.Hide;
begin
  inherited;

end;

function TSquare.PointInFugure(aX, aY: integer): boolean;
begin
  result := not((aX > X + Size) or (aY > Y + Size) or (aX < X - Size) or
      (aY < Y - Size))
end;

end.
