{$mode objfpc}{$H+}
unit ws_linux_gtk4;

interface

uses
  rtl_sys, events, classes, controls, forms, stdctrls, ws_intf;

type
  // GTK4 Basis-Typen (Inline-Bindings)
  PGObject = Pointer;
  PGtkWidget = Pointer;
  PGtkWindow = Pointer;
  PGtkButton = Pointer;
  PGtkApplicationWindow = Pointer;
  PGtkApplication = Pointer;
  PGtkBox = Pointer;
  gpointer = Pointer;
  gint = Integer;
  gboolean = Boolean;
  gchar = Char;
  Pgchar = PChar;
  
  // GTK4 Function-Pointer Typen
  TGtkSignalCallback = procedure(widget: PGtkWidget; user_data: gpointer); cdecl;
  TGtkActivateCallback = procedure(app: PGtkApplication; user_data: gpointer); cdecl;

  // GTK4 WidgetSet Implementation
  TGtk4WidgetSet = class(TWidgetSet)
  private
    FApplication: PGtkApplication;
    FTerminated: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    
    procedure Initialize; override;
    procedure Run; override; 
    procedure ProcessMessages; override;
    procedure Terminate; override;
    
    // Direkte Control-Methoden
    procedure CreateControlHandle(AControl: TWinControl); override;
    procedure DestroyControlHandle(AControl: TWinControl); override;
    procedure SetControlBounds(AControl: TWinControl; ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetControlVisible(AControl: TWinControl; AVisible: Boolean); override;
    procedure SetControlEnabled(AControl: TWinControl; AEnabled: Boolean); override;
    
    // Form-spezifische Methoden
    procedure SetFormCaption(AForm: TObject; const ACaption: string); override;
    procedure ShowForm(AForm: TObject); override;
    procedure HideForm(AForm: TObject); override;
    
    // Button-spezifische Methoden
    procedure SetButtonCaption(AButton: TObject; const ACaption: string); override;
  end;
  
  // GTK4 WinControl Implementation
  TGtk4WSWinControl = class(TWSWinControl)
  protected
    FWidget: PGtkWidget;
  public
    destructor Destroy; override;
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetVisible(AVisible: Boolean); override;
    procedure SetEnabled(AEnabled: Boolean); override;
  end;
  
  // GTK4 Form Implementation
  TGtk4WSForm = class(TGtk4WSWinControl)
  private
    FMainBox: PGtkBox;
  public
    procedure CreateHandle; override;
    procedure SetCaption(const ACaption: string);
    procedure Show;
    procedure Hide;
  end;
  
  // GTK4 Button Implementation  
  TGtk4WSButton = class(TGtk4WSWinControl)
  public
    procedure CreateHandle; override;
    procedure SetCaption(const ACaption: string);
  end;

// GTK4 Inline-Bindings (external library declarations)
function gtk_application_new(application_id: Pgchar; flags: gint): PGtkApplication; cdecl; external 'gtk-4';
function g_application_run(application: PGtkApplication; argc: gint; argv: Pointer): gint; cdecl; external 'gio-2.0';
procedure g_application_quit(application: PGtkApplication); cdecl; external 'gio-2.0';
procedure g_application_hold(application: PGtkApplication); cdecl; external 'gio-2.0';
procedure g_application_release(application: PGtkApplication); cdecl; external 'gio-2.0';
procedure gtk_init; cdecl; external 'gtk-4';
function g_signal_connect_data(instance: gpointer; detailed_signal: Pgchar; 
  c_handler: Pointer; data: gpointer; destroy_data: Pointer; connect_flags: gint): gint; cdecl; external 'gtk-4';

function gtk_application_window_new(application: PGtkApplication): PGtkApplicationWindow; cdecl; external 'gtk-4';
procedure gtk_window_set_title(window: PGtkWindow; title: Pgchar); cdecl; external 'gtk-4';
procedure gtk_window_set_default_size(window: PGtkWindow; width, height: gint); cdecl; external 'gtk-4';
procedure gtk_window_present(window: PGtkWindow); cdecl; external 'gtk-4';
procedure gtk_widget_hide(widget: PGtkWidget); cdecl; external 'gtk-4';
procedure gtk_widget_set_visible(widget: PGtkWidget; visible: gboolean); cdecl; external 'gtk-4';
procedure gtk_widget_set_sensitive(widget: PGtkWidget; sensitive: gboolean); cdecl; external 'gtk-4';

function gtk_box_new(orientation: gint; spacing: gint): PGtkBox; cdecl; external 'gtk-4';
procedure gtk_box_append(box: PGtkBox; child: PGtkWidget); cdecl; external 'gtk-4';
procedure gtk_window_set_child(window: PGtkWindow; child: PGtkWidget); cdecl; external 'gtk-4';

function gtk_button_new_with_label(alabel: Pgchar): PGtkButton; cdecl; external 'gtk-4';
procedure gtk_button_set_label(button: PGtkButton; alabel: Pgchar); cdecl; external 'gtk-4';

procedure g_object_set_data(obj: PGObject; key: Pgchar; data: gpointer); cdecl; external 'gtk-4';
function g_object_get_data(obj: PGObject; key: Pgchar): gpointer; cdecl; external 'gtk-4';

function g_main_context_iteration(context: Pointer; may_block: gboolean): gboolean; cdecl; external 'gtk-4';

// GTK4 Konstanten
const
  GTK_ORIENTATION_VERTICAL = 1;
  G_CONNECT_AFTER = 1;

// Globale Variablen für GTK4 Event-Handling
var
  Gtk4WidgetSet: TGtk4WidgetSet = nil;

implementation

// Globale Referenz für Main Form (für GTK4 activate handler)
var
  GlobalMainForm: TWinControl = nil;

// Forward-Deklarationen für Event-Handler
procedure OnGtkButtonClicked(widget: PGtkWidget; user_data: gpointer); cdecl; forward;
procedure OnWindowClose(widget: PGtkWidget; user_data: gpointer); cdecl; forward;
function OnMouseEvent(widget: PGtkWidget; event: Pointer; user_data: gpointer): gboolean; cdecl; forward;

// Helper-Funktion für g_signal_connect 
function g_signal_connect(instance: gpointer; detailed_signal: Pgchar; 
  c_handler: Pointer; data: gpointer): gint;
begin
  try
    if Assigned(instance) and Assigned(c_handler) then
      Result := g_signal_connect_data(instance, detailed_signal, c_handler, data, nil, 0)
    else
      Result := 0;
  except
    Result := 0;
  end;
end;

// Mouse Event Handler (für Form Mouse-Events)
function OnMouseEvent(widget: PGtkWidget; event: Pointer; user_data: gpointer): gboolean; cdecl;
begin
  // Mouse-Events sicher abfangen und ignorieren für PoC
  // Rückgabe FALSE = Event nicht behandelt (lasse GTK4 weiter verarbeiten)
  Result := False;
end;

// Window Close Event Handler
procedure OnWindowClose(widget: PGtkWidget; user_data: gpointer); cdecl;
var
  Control: TObject;
begin
  try
    WriteLn('Window Close Event empfangen');
    
    // Hole Control-Objektreferenz
    Control := TObject(g_object_get_data(PGObject(widget), 'mini_lcl_obj'));
    
    if Assigned(Control) and (Control is TForm) then
    begin
      WriteLn('Form wird geschlossen: ', TForm(Control).Caption);
      TForm(Control).Close;
    end;
    
    // Release Application Hold und beende
    if Assigned(Gtk4WidgetSet) and Assigned(Gtk4WidgetSet.FApplication) then
    begin
      WriteLn('Release GTK4 Application Hold...');
      g_application_release(Gtk4WidgetSet.FApplication);
      
      WriteLn('Beende GTK4 Application...');
      g_application_quit(Gtk4WidgetSet.FApplication);
    end;
  except
    on E: Exception do
      WriteLn('FEHLER in OnWindowClose: ', E.Message);
  end;
end;

// Button Click Event Handler
procedure OnGtkButtonClicked(widget: PGtkWidget; user_data: gpointer); cdecl;
var
  Control: TObject;
begin
  try
    // Hole Control-Objektreferenz aus GTK Widget
    Control := TObject(g_object_get_data(PGObject(widget), 'mini_lcl_obj'));
    
    if Assigned(Control) and (Control is TButton) then
    begin
      TButton(Control).Click;  // Rufe Pascal OnClick Event auf
    end;
  except
    on E: Exception do
      WriteLn('FEHLER in OnGtkButtonClicked: ', E.Message);
  end;
end;

// Prozedur zum Iterieren und Hinzufügen von Controls zu einer GTK-Box
procedure IterateAndAddControls(AControl: TWinControl; AGtkParentBox: PGtkBox);
var
  I: Integer;
  LChildControl: TControl;
  LWSButton: TGtk4WSButton;
  LButtonWidget: PGtkButton;
  LControlWS: TObject;
begin
  if not Assigned(AControl) or not Assigned(AGtkParentBox) then
    Exit;

  WriteLn('Iteriere Controls für Parent: ', AControl.ClassName);

  // Durchlaufe alle Kind-Controls des übergebenen AControl
  for I := 0 to AControl.ControlCount - 1 do
  begin
    LChildControl := AControl.Controls[I];
    WriteLn('  Verarbeite Kind-Control: ', LChildControl.ClassName);

    if LChildControl is TButton then
    begin
      // Hole das WidgetSet-Objekt aus dem Pascal-Control (TButton ist ein TWinControl)
      LControlWS := TWinControl(LChildControl).FWSControl;
      if not Assigned(LControlWS) then
      begin
        WriteLn('    FEHLER: FWSControl nicht zugewiesen für ', LChildControl.ClassName);
        Continue;
      end;
      
      LWSButton := TGtk4WSButton(TWSWinControl(LControlWS));
      
      // Erstelle das GTK Button Widget
      LButtonWidget := gtk_button_new_with_label(PChar(TButton(LChildControl).Caption));
      if Assigned(LButtonWidget) then
      begin
        LWSButton.FWidget := PGtkWidget(LButtonWidget);
        TWinControl(LChildControl).FHandle := PGtkWidget(LButtonWidget);

        // Speichere die Pascal-Control-Referenz im GTK-Widget
        g_object_set_data(PGObject(LButtonWidget), 'mini_lcl_obj', gpointer(LChildControl));

        // TEMPORÄR DEAKTIVIERT: Verbinde das 'clicked'-Signal
        // g_signal_connect(LButtonWidget, 'clicked', @OnGtkButtonClicked, nil);

        // Füge den Button zur Parent-Box hinzu
        gtk_box_append(AGtkParentBox, PGtkWidget(LButtonWidget));
        WriteLn('    GTK4 Button erstellt und hinzugefügt: ' + TButton(LChildControl).Caption);
      end
      else
        WriteLn('    FEHLER: GTK4 Button-Erstellung für ' + TButton(LChildControl).Caption + ' fehlgeschlagen.');
    end
    // Hier können weitere Control-Typen hinzugefügt werden (z.B. TLabel, TEdit)
  end;
end;

// GTK4 Startup Handler - wird vor activate aufgerufen
procedure OnGtkStartup(app: PGtkApplication; user_data: gpointer); cdecl;
begin
  try
    WriteLn('GTK4 Application startup - bereit für Window-Erstellung');
  except
    on E: Exception do
      WriteLn('FEHLER in OnGtkStartup: ', E.Message);
  end;
end;

// GTK4 Activate Handler - wird aufgerufen wenn Application aktiviert wird
procedure OnGtkActivate(app: PGtkApplication; user_data: gpointer); cdecl;
var
  LForm: TForm;
  LWSForm: TGtk4WSForm;
  LMainBox: PGtkBox;
  LWidget: PGtkWidget;
  LFormWS: TObject;
begin
  try
    WriteLn('GTK4 Application aktiviert - beginne Widget-Erstellung.');
    
    if Assigned(GlobalMainForm) and (GlobalMainForm is TForm) then
    begin
      LForm := TForm(GlobalMainForm);
      
      // Hole das WidgetSet-Objekt aus der Pascal-Form
      LFormWS := LForm.FWSControl;
      if not Assigned(LFormWS) then
      begin
        WriteLn('FEHLER: FWSControl nicht zugewiesen für MainForm');
        Exit;
      end;
      
      LWSForm := TGtk4WSForm(TWSWinControl(LFormWS));

      // Erstelle GTK ApplicationWindow
      LWidget := PGtkWidget(gtk_application_window_new(app));
      if not Assigned(LWidget) then
      begin
        WriteLn('FEHLER: gtk_application_window_new returned null in OnGtkActivate');
        Exit;
      end;
      LWSForm.FWidget := LWidget; // Setze das GTK Widget im WidgetSet Objekt
      TWinControl(LForm).FHandle := LWidget;   // Setze das Handle in der Pascal TForm Instanz

      WriteLn('GTK4 ApplicationWindow für MainForm erstellt.');

      // Setze Titel und Standardgröße
      gtk_window_set_title(PGtkWindow(LWidget), PChar(LForm.Caption));
      gtk_window_set_default_size(PGtkWindow(LWidget), LForm.Width, LForm.Height);

      // Erstelle Hauptcontainer Box
      LMainBox := gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
      if not Assigned(LMainBox) then
      begin
        WriteLn('FEHLER: GtkBox-Erstellung fehlgeschlagen in OnGtkActivate');
        Exit;
      end;
      LWSForm.FMainBox := LMainBox; // Speichere MainBox Referenz im WidgetSet Form Objekt

      // Setze die Box als Kind des Fensters
      gtk_window_set_child(PGtkWindow(LWidget), PGtkWidget(LMainBox));
      WriteLn('GtkBox als Kind des Fensters gesetzt.');

      // Speichere Referenzen im GTK Widget
      g_object_set_data(PGObject(LWidget), 'mini_lcl_obj', gpointer(LForm));
      g_object_set_data(PGObject(LWidget), 'main_box', gpointer(LMainBox));

      // TEMPORÄR DEAKTIVIERT: Verbinde Signale
      // g_signal_connect(LWidget, 'close-request', @OnWindowClose, nil);

      WriteLn('Signale für MainForm temporär deaktiviert.');

      // Halte Application aktiv (jetzt, wo Widgets erstellt sind)
      g_application_hold(app);
      WriteLn('GTK4 Application wird aktiv gehalten.');

      // Zeige das Fenster an
      gtk_window_present(PGtkWindow(LWidget));
      WriteLn('MainForm über GTK activate-Signal angezeigt.');
      
      // Füge Kind-Controls (Buttons) hinzu
      IterateAndAddControls(LForm, LMainBox);

    end
    else
    begin
      WriteLn('FEHLER: GlobalMainForm nicht zugewiesen oder keine TForm Instanz.');
    end;
  except
    on E: Exception do
      WriteLn('FEHLER in OnGtkActivate: ', E.Message);
  end;
end;

// TGtk4WidgetSet Implementation
constructor TGtk4WidgetSet.Create;
begin
  inherited Create;
  FApplication := nil;
  FTerminated := False;
end;

destructor TGtk4WidgetSet.Destroy;
begin
  if Assigned(FApplication) then
  begin
    FApplication := nil;
  end;
  inherited Destroy;
end;

procedure TGtk4WidgetSet.Initialize;
begin
  gtk_init;
  WriteLn('GTK4 initialisiert.');
  
  FApplication := gtk_application_new('com.minilcl.demo', 0);
  if not Assigned(FApplication) then
    raise Exception.Create('Failed to create GTK4 application');
    
  WriteLn('GTK4 Application erstellt: ', Assigned(FApplication));
  
  g_signal_connect(FApplication, 'startup', @OnGtkStartup, nil);
  g_signal_connect(FApplication, 'activate', @OnGtkActivate, nil);
  
  WriteLn('GTK4 WidgetSet initialisiert.');
end;

procedure TGtk4WidgetSet.Run;
var
  ExitCode: gint;
begin
  if Assigned(FApplication) then
  begin
    WriteLn('Starte GTK4 Hauptschleife...');
    ExitCode := g_application_run(FApplication, 0, nil);
    WriteLn('GTK4 Hauptschleife beendet mit Exit-Code: ', ExitCode);
  end
  else
    WriteLn('FEHLER: Keine GTK4 Application verfügbar für Run');
end;

procedure TGtk4WidgetSet.ProcessMessages;
begin
  g_main_context_iteration(nil, False);
end;

procedure TGtk4WidgetSet.Terminate;
begin
  FTerminated := True;
  if Assigned(FApplication) then
    g_application_quit(FApplication);
end;

// TGtk4WSWinControl Implementation
destructor TGtk4WSWinControl.Destroy;
begin
  DestroyHandle;
  inherited Destroy;
end;

procedure TGtk4WSWinControl.CreateHandle;
begin
  FWidget := nil;
end;

procedure TGtk4WSWinControl.DestroyHandle;
begin
  if Assigned(FWidget) then
  begin
    FWidget := nil;
  end;
end;

procedure TGtk4WSWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  // TODO: GTK4 Layout-Management
end;

procedure TGtk4WSWinControl.SetVisible(AVisible: Boolean);
begin
  if Assigned(FWidget) then
    gtk_widget_set_visible(FWidget, AVisible);
end;

procedure TGtk4WSWinControl.SetEnabled(AEnabled: Boolean);
begin
  if Assigned(FWidget) then
    gtk_widget_set_sensitive(FWidget, AEnabled);
end;

// TGtk4WSForm Implementation
procedure TGtk4WSForm.CreateHandle;
begin
  FWidget := nil;
  
  if not Assigned(GlobalMainForm) and Assigned(Control) and (Control is TForm) then
  begin
    GlobalMainForm := TWinControl(Control);
    WriteLn('Pascal MainForm Referenz gespeichert für GTK4 activate-Signal.');
  end;
  
  TWinControl(Control).FHandle := nil;
  WriteLn('TGtk4WSForm.CreateHandle aufgerufen - GTK Widget-Erstellung verzögert.');
end;

procedure TGtk4WSForm.SetCaption(const ACaption: string);
begin
  if Assigned(FWidget) then
    gtk_window_set_title(PGtkWindow(FWidget), PChar(ACaption));
end;

procedure TGtk4WSForm.Show;
begin
  if Assigned(FWidget) then
  begin
    gtk_window_present(PGtkWindow(FWidget));
    WriteLn('GTK4 Form angezeigt.');
  end;
end;

procedure TGtk4WSForm.Hide;
begin
  if Assigned(FWidget) then
    gtk_widget_hide(FWidget);
end;

// TGtk4WSButton Implementation
procedure TGtk4WSButton.CreateHandle;
begin
  FWidget := nil;
  WriteLn('TGtk4WSButton.CreateHandle aufgerufen - GTK Widget-Erstellung verzögert.');
end;

procedure TGtk4WSButton.SetCaption(const ACaption: string);
begin
  if Assigned(FWidget) then
    gtk_button_set_label(PGtkButton(FWidget), PChar(ACaption));
end;

// TGtk4WidgetSet direkte Control-Methoden
procedure TGtk4WidgetSet.CreateControlHandle(AControl: TWinControl);
var
  WSControl: TWSWinControl;
begin
  if not Assigned(AControl.Handle) and not Assigned(AControl.FWSControl) then
  begin
    if AControl is TForm then
    begin
      WSControl := TGtk4WSForm.Create(AControl);
      TGtk4WSForm(WSControl).CreateHandle;
    end
    else if AControl is TButton then
    begin
      WSControl := TGtk4WSButton.Create(AControl);
      TGtk4WSButton(WSControl).CreateHandle;
    end;
    
    if Assigned(WSControl) then
    begin
      AControl.FWSControl := WSControl;
      WriteLn('FWSControl zugewiesen für: ', AControl.ClassName);
    end;
  end;
end;

procedure TGtk4WidgetSet.DestroyControlHandle(AControl: TWinControl);
begin
  if Assigned(AControl.Handle) then
  begin
    // GTK Widgets werden automatisch durch Parent zerstört
  end;
end;

procedure TGtk4WidgetSet.SetControlBounds(AControl: TWinControl; ALeft, ATop, AWidth, AHeight: Integer);
begin
  // TODO: GTK4 Layout-Management
end;

procedure TGtk4WidgetSet.SetControlVisible(AControl: TWinControl; AVisible: Boolean);
begin
  if Assigned(AControl.Handle) then
    gtk_widget_set_visible(PGtkWidget(AControl.Handle), AVisible);
end;

procedure TGtk4WidgetSet.SetControlEnabled(AControl: TWinControl; AEnabled: Boolean);
begin
  if Assigned(AControl.Handle) then
    gtk_widget_set_sensitive(PGtkWidget(AControl.Handle), AEnabled);
end;

// Form-spezifische Methoden
procedure TGtk4WidgetSet.SetFormCaption(AForm: TObject; const ACaption: string);
begin
  if Assigned(AForm) and (AForm is TWinControl) and Assigned(TWinControl(AForm).Handle) then
    gtk_window_set_title(PGtkWindow(TWinControl(AForm).Handle), PChar(ACaption));
end;

procedure TGtk4WidgetSet.ShowForm(AForm: TObject);
begin
  if Assigned(AForm) and (AForm is TWinControl) and Assigned(TWinControl(AForm).Handle) then
    gtk_window_present(PGtkWindow(TWinControl(AForm).Handle));
end;

procedure TGtk4WidgetSet.HideForm(AForm: TObject);
begin
  if Assigned(AForm) and (AForm is TWinControl) and Assigned(TWinControl(AForm).Handle) then
    gtk_widget_hide(PGtkWidget(TWinControl(AForm).Handle));
end;

// Button-spezifische Methoden
procedure TGtk4WidgetSet.SetButtonCaption(AButton: TObject; const ACaption: string);
begin
  if Assigned(AButton) and (AButton is TWinControl) and Assigned(TWinControl(AButton).Handle) then
    gtk_button_set_label(PGtkButton(TWinControl(AButton).Handle), PChar(ACaption));
end;

end.
