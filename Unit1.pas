unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, FigureClass, ComCtrls, ToolWin, Menus, ImgList;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Button1: TButton;
    Panel2: TPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button2: TButton;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Label2: TLabel;
    Edit3: TEdit;
    Label3: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ToolButton5: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure Form1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  FigureList: TList;
  ClickPos: TPoint;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FreeAndNil(FigureList);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FigureList := TList.Create;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
    Label1.Color := ColorDialog1.Color;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if not Assigned(FigureList) then
    FigureList := TList.Create;
  FigureList.DeleteSelectedItem;
end;

procedure TForm1.Button4Click(Sender: TObject);
var nx, ny: integer;
begin
Form1.Canvas.MoveTo(300, 300);
nx := 300; ny := 300;
nx := nx + Round(Cos(36 / 180 * pi) * 100);  ny := ny - Round(Sin(36 / 180 * pi) * 100);
Form1.Canvas.LineTo(nx, ny);
nx := nx + Round(Cos(252 / 180 * pi) * 100);  ny := ny - Round(Sin(252 / 180 * pi) * 100);
Form1.Canvas.LineTo(nx, ny);
nx := nx + Round(Cos(108 / 180 * pi) * 100);  ny := ny - Round(Sin(108 / 180 * pi) * 100);
Form1.Canvas.LineTo(nx, ny);
nx := nx + Round(Cos(324 / 180 * pi) * 100);  ny := ny - Round(Sin(324 / 180 * pi) * 100);
Form1.Canvas.LineTo(nx, ny);
nx := nx + Round(Cos(180 / 180 * pi) * 100);  ny := ny - Round(Sin(180 / 180 * pi) * 100);
Form1.Canvas.LineTo(nx, ny);
end;

procedure TForm1.Button5Click(Sender: TObject);
var i, r, x, y, step, angle: integer;
Vertex: TPoint;
VertexCount: integer;

begin
r := 100;
VertexCount := 5;
Form1.Canvas.Brush.Style := bsClear;
//Form1.Canvas.Ellipse(300 - r, 300 - r, 300 + r, 300 + r);
//Form1.Canvas.Ellipse(300 - Round(r / 3), 300 - Round(r / 3), 300 + Round(r / 3), 300 + Round(r / 3));
Vertex.X := 300; Vertex.Y := 300 + r;
x := Vertex.X; y := Vertex.Y;
Form1.Canvas.MoveTo(Vertex.X, Vertex.Y);
step := Round(360 / (VertexCount * 2));
angle := 270;
for i :=  1 to VertexCount * 2 do
  begin
  if (i mod 2 = 0) then
  begin
   x := 300 + Round(Cos((step * i + angle) / 180 * pi) * r);  y := 300 - Round(Sin((step * i + angle) / 180 * pi) * r);
  end
   else
   begin
   x := 300 + Round(Cos((step * i + angle) / 180 * pi) * (r / 3));  y := 300 - Round(Sin((step * i + angle) / 180 * pi) * (r / 3));
   end;

  Sleep(50);
  Form1.Canvas.LineTo(x, y);
  end;
end;

procedure TForm1.Form1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Point: TDot;
  Circle: TCircle;
  Square: TSquare;
  Star: TStar;
  ListItem, NextItem: PListItem;
begin
  ClickPos.X := X;
  ClickPos.Y := Y;
  if not Assigned(FigureList) then
    FigureList := TList.Create;
  if Button = mbLeft then
  begin
    if ToolButton1.Down then
    begin

    end;
    if ToolButton2.Down then
    begin
      Point := TDot.Create(X, Y, Label1.Color, Form1.Canvas);
      FigureList.AddItem(Point);
      // Point.Draw;
    end;
    if ToolButton3.Down then
    begin
      Circle := TCircle.Create(X, Y, Label1.Color, Form1.Canvas, StrToInt(Edit2.Text));
      FigureList.AddItem(Circle);
      // Circle.Draw;
    end;
    if ToolButton4.Down then
    begin
      Square := TSquare.Create(X, Y, Label1.Color, Form1.Canvas, StrToInt(Edit2.Text));
      FigureList.AddItem(Square);
      // Square.Draw;
    end;
  if ToolButton5.Down then
    begin
      Star := TStar.Create(X, Y, Label1.Color, Form1.Canvas, StrToInt(Edit2.Text), StrToInt(Edit3.Text));
      FigureList.AddItem(Star);
      // Square.Draw;
    end;
    if FigureList.FirstItem = nil then
      exit;
    ListItem := FigureList.FirstItem;
    repeat
      NextItem := ListItem.NextItem;
      ListItem.Item.Selected := ListItem.Item.PointInFugure(X, Y);
      if ListItem.Item.Selected then Edit1.Text := ListItem.Item.Info;
      ListItem := NextItem;
    until ListItem = nil;
  end;
end;

procedure TForm1.N1Click(Sender: TObject);
var
  ListItem, NextItem: PListItem;
begin
  if FigureList.FirstItem = nil then
    exit;
  ListItem := FigureList.FirstItem;
  repeat
    if ListItem.Item.Selected then
      ListItem.Item.Shift(ClickPos.X, ClickPos.Y);
    ListItem := ListItem.NextItem;
  until ListItem = nil;
end;

procedure TForm1.N2Click(Sender: TObject);
var
  CurPos: TPoint;
begin
  GetCursorPos(CurPos);
  ShowMessage(IntToStr(CurPos.X) + ' ' + IntToStr(CurPos.Y));
end;

end.
