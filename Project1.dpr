program Project1;

uses
  FastMM4 in 'FastMM4.pas',
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  FigureClass in 'FigureClass.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  ABOUT in 'ABOUT.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
