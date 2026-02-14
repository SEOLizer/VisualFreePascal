program demo_minilcl;

{$mode objfpc}{$H+}

uses
  rtl_sys, events, classes, controls, forms, stdctrls, ws_intf, ws_linux_gtk4;

type
  { TMainForm }
  TMainForm = class(TForm)
  private
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  // Form-Eigenschaften
  Caption := 'Mini-LCL Demo mit GTK4';
  Width := 400;
  Height := 300;
  
  // Button erstellen
  Button1 := TButton.Create(Self);
  Button1.Parent := Self;
  Button1.Caption := 'Klick mich!';
  Button1.Width := 100;
  Button1.Height := 30;
  Button1.OnClick := @Button1Click;
  
  WriteLn('MainForm erstellt.');
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  NewCaption: string;
begin
  try
    WriteLn('*** BUTTON WURDE GEKLICKT! ***');
    
    if Assigned(Sender) then
      WriteLn('Sender: ', Sender.ClassName)
    else
      WriteLn('Sender: NIL');
      
    if Assigned(Button1) then
      WriteLn('Button Caption: ', Button1.Caption)
    else
      WriteLn('Button1: NIL');
    
    // Ändere Button-Text nach Klick
    if Assigned(Button1) then
    begin
      if Button1.Caption = 'Klick mich!' then
        NewCaption := 'Geklickt!'
      else
        NewCaption := 'Klick mich!';
      
      Button1.Caption := NewCaption;
      
      // Aktualisiere GTK4 Button direkt
      if Assigned(Gtk4WidgetSet) then
      begin
        Gtk4WidgetSet.SetButtonCaption(Button1, NewCaption);
        WriteLn('Button Caption geändert zu: ', NewCaption);
      end;
    end;
    
    WriteLn('*** Button Click Event abgeschlossen ***');
  except
    on E: Exception do
      WriteLn('FEHLER in Button1Click: ', E.Message);
  end;
end;

// Bridge-Objekt für WidgetSet-Kompatibilität
type
  TWidgetSetBridge = class(forms.TWidgetSet)
  private
    FGtk4WidgetSet: TGtk4WidgetSet;
  public
    constructor Create(AGtk4WidgetSet: TGtk4WidgetSet);
    procedure Initialize; override;
    procedure Run; override;
    procedure ProcessMessages; override;
    procedure SetFormCaption(AForm: TObject; const ACaption: string); override;
    procedure ShowForm(AForm: TObject); override;
    procedure HideForm(AForm: TObject); override;
    procedure Terminate; override;
    
    // Control-Handle Management
    procedure CreateFormHandle(AForm: TForm);
    procedure CreateControlHandles(AForm: TForm);
  end;

constructor TWidgetSetBridge.Create(AGtk4WidgetSet: TGtk4WidgetSet);
begin
  inherited Create;
  FGtk4WidgetSet := AGtk4WidgetSet;
end;

procedure TWidgetSetBridge.Initialize;
begin
  if Assigned(FGtk4WidgetSet) then
    FGtk4WidgetSet.Initialize;
end;

procedure TWidgetSetBridge.Run;
begin
  if Assigned(FGtk4WidgetSet) then
    FGtk4WidgetSet.Run;
end;

procedure TWidgetSetBridge.ProcessMessages;
begin
  if Assigned(FGtk4WidgetSet) then
    FGtk4WidgetSet.ProcessMessages;
end;

procedure TWidgetSetBridge.SetFormCaption(AForm: TObject; const ACaption: string);
begin
  if Assigned(FGtk4WidgetSet) then
    FGtk4WidgetSet.SetFormCaption(AForm, ACaption);
end;

procedure TWidgetSetBridge.ShowForm(AForm: TObject);
begin
  if Assigned(FGtk4WidgetSet) then
    FGtk4WidgetSet.ShowForm(AForm);
end;

procedure TWidgetSetBridge.HideForm(AForm: TObject);
begin
  if Assigned(FGtk4WidgetSet) then
    FGtk4WidgetSet.HideForm(AForm);
end;

procedure TWidgetSetBridge.Terminate;
begin
  if Assigned(FGtk4WidgetSet) then
    FGtk4WidgetSet.Terminate;
end;

procedure TWidgetSetBridge.CreateFormHandle(AForm: TForm);
begin
  if Assigned(FGtk4WidgetSet) and Assigned(AForm) then
  begin
    FGtk4WidgetSet.CreateControlHandle(AForm);
    if Assigned(AForm.Handle) then
    begin
      FGtk4WidgetSet.SetFormCaption(AForm, AForm.Caption);
      WriteLn('Form Handle erstellt: ', AForm.Caption);
    end;
  end;
end;

procedure TWidgetSetBridge.CreateControlHandles(AForm: TForm);
var
  i: Integer;
  Control: TControl;
begin
  if not Assigned(AForm) then Exit;
  
  // Erstelle Handles für alle Child-Controls
  for i := 0 to AForm.ControlCount - 1 do
  begin
    Control := AForm.Controls[i];
    if Control is TWinControl then
    begin
      FGtk4WidgetSet.CreateControlHandle(TWinControl(Control));
      if Assigned(TWinControl(Control).Handle) then
        WriteLn('Control Handle erstellt: ', Control.ClassName);
    end;
  end;
end;

var
  WidgetSetBridge: TWidgetSetBridge;

begin
  WriteLn('=== Mini-LCL Demo startet ===');
  
  // Erstelle GTK4 WidgetSet
  Gtk4WidgetSet := TGtk4WidgetSet.Create;
  
  // Erstelle Bridge für Typen-Kompatibilität
  WidgetSetBridge := TWidgetSetBridge.Create(Gtk4WidgetSet);
  forms.WidgetSet := WidgetSetBridge;
  
  WriteLn('WidgetSet erstellt: ', Gtk4WidgetSet.ClassName);
  
  // Initialisiere Application über Application-Objekt
  Application.Initialize;
  
  // Erstelle MainForm über Application
  Application.CreateForm(TMainForm, MainForm);
  
  WriteLn('MainForm-Referenz: ', Assigned(MainForm));
  WriteLn('Application.MainForm: ', Assigned(Application.MainForm));
  
  // Teste nur Form-Erstellung OHNE Button und Container
  if Assigned(MainForm) and Assigned(WidgetSetBridge) then
  begin
    WidgetSetBridge.CreateFormHandle(MainForm);
    WriteLn('Form Handle erstellt - nun auch Control Handles erstellen');
    WidgetSetBridge.CreateControlHandles(MainForm);
  end;
  
  // Starte Hauptschleife über Application
  WriteLn('Rufe Application.Run auf...');
  Application.Run;
  
  WriteLn('=== Demo beendet ===');
end.