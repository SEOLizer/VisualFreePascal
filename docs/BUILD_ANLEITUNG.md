# Mini-LCL Build-Anleitung

## Übersicht
Dieses Mini-LCL Framework ist ein Proof-of-Concept für eine plattformunabhängige GUI-Bibliothek in FreePascal mit GTK4-Backend.

## Systemanforderungen

### Linux (Ubuntu/Debian)
1. **FreePascal Compiler:**
   ```bash
   sudo apt update
   sudo apt install fpc
   ```

2. **GTK4 Development Libraries:**
   ```bash
   sudo apt install libgtk-4-dev pkg-config
   ```

3. **Weitere Abhängigkeiten (optional):**
   ```bash
   sudo apt install build-essential
   ```

### Andere Linux-Distributionen
- **Red Hat/Fedora/CentOS:**
  ```bash
  sudo dnf install fpc gtk4-devel pkg-config
  ```

- **Arch Linux:**
  ```bash
  sudo pacman -S fpc gtk4 pkg-config
  ```

## Build-Prozess

### 1. Kompilierung
```bash
# Im vfp-Verzeichnis mit GTK4-Libraries:
fpc -k'-lgtk-4 -lgio-2.0 -lgobject-2.0 -lglib-2.0' demo_minilcl.lpr
```

### 2. Alternative Kompilierung
Falls pkg-config verfügbar ist:

```bash
# GTK4 Flags anzeigen:
pkg-config --cflags --libs gtk4

# Kompilierung mit pkg-config (experimentell):
fpc -k"`pkg-config --libs gtk4`" demo_minilcl.lpr
```

**Wichtig:** Die erste Methode ist getestet und funktioniert zuverlässig!

### 3. Ausführung
```bash
# Demo starten (läuft bis Fenster geschlossen wird):
./demo_minilcl

# Im Hintergrund starten:
./demo_minilcl &

# Mit Debug-Output:
./demo_minilcl 2>&1 | tee demo.log
```

**Erwartetes Verhalten:**
- GTK4-Fenster erscheint mit Titel "Mini-LCL Demo mit GTK4"  
- Button "Klick mich!" ist sichtbar
- Button-Clicks wechseln Caption zwischen "Klick mich!" und "Geklickt!"
- Fenster schließen beendet das Programm korrekt

## Dateistruktur

```
vfp/
├── rtl_sys.pas           # Basistypen, Exceptions, Listen
├── events.pas            # Event-Definitionen (TNotifyEvent)
├── classes.pas           # TPersistent, TComponent-Hierarchie
├── controls.pas          # TControl, TWinControl (plattformunabhängig)
├── forms.pas            # TApplication, TForm
├── stdctrls.pas         # TButton und andere Standard-Controls
├── ws_intf.pas          # WidgetSet-Interface (abstrakt)
├── ws_linux_gtk4.pas    # GTK4-Backend Implementation
├── demo_minilcl.lpr     # Demo-Programm
└── BUILD_ANLEITUNG.md   # Diese Anleitung
```

## Architektur

### Schichtenmodell
1. **RTL-Basis:** `rtl_sys`, `events`, `classes`
2. **Control-Framework:** `controls`, `forms`, `stdctrls` 
3. **WidgetSet-Interface:** `ws_intf`
4. **Platform-Backend:** `ws_linux_gtk4`

### Kernkonzepte
- **Plattformunabhängigkeit:** Application-Code sieht nur TForm, TButton etc.
- **WidgetSet-Pattern:** Platform-Details über TGtk4WidgetSet gekapselt  
- **Handle-System:** TWinControl.Handle als opaker Platform-Widget-Pointer
- **Event-Bridge:** GTK4-Signale → Pascal OnClick Events

## Demo-Funktionen

Das Demo-Programm zeigt:
- **Fenster:** 400x300 Pixel GTK4 ApplicationWindow mit Titel
- **Button:** Mit Caption "Klick mich!" (wechselt zu "Geklickt!" beim Click)
- **Event-Handling:** Button-Click → WriteLn + Caption-Änderung  
- **Application Lifecycle:** Läuft bis Fenster geschlossen wird
- **Window Close:** Strg+C oder Fenster schließen beendet das Programm
- **GTK4-Integration:** Vollständige GTK4 Application mit Hold/Release

## Troubleshooting

### Kompilierung schlägt fehl
```
Error: -lgtk-4 kann nicht gefunden werden
```
**Lösung:** GTK4 Development Package installieren:
```bash
sudo apt install libgtk-4-dev libglib2.0-dev
```

### Linking-Fehler bei GTK4-Libraries
```
undefined reference to symbol 'g_main_context_iteration'
```
**Lösung:** Verwende die vollständigen Library-Flags:
```bash
fpc -k'-lgtk-4 -lgio-2.0 -lgobject-2.0 -lglib-2.0' demo_minilcl.lpr
```

### Runtime-Fehler
```
Failed to create GTK4 application
```
**Lösung:** GTK4 Runtime installieren:
```bash
sudo apt install libgtk-4-1
```

### Linking-Probleme
Falls automatische Library-Erkennung fehlschlägt:
```bash
# Manuelle Library-Pfade:
fpc -Fl/usr/lib/x86_64-linux-gnu -l demo_minilcl.lpr

# Oder mit pkg-config:
fpc -l -k"`pkg-config --libs gtk4`" demo_minilcl.lpr
```

## Erweiterungen

### Neue Controls hinzufügen
1. Definiere TMyControl in entsprechender Unit (z.B. stdctrls.pas)
2. Implementiere TGtk4WSMyControl in ws_linux_gtk4.pas
3. Erweitere TGtk4WidgetSet.CreateControlHandle für neuen Control-Typ

### Andere Platforms
1. Erstelle ws_windows_win32.pas oder ws_linux_qt.pas
2. Implementiere TWidgetSet-Subklasse für neue Platform
3. Registriere WidgetSet im Demo-Programm

## Beispiel-Nutzung

```pascal
program mein_demo;
uses forms, stdctrls, ws_linux_gtk4;

var
  Form: TForm;
  Button: TButton;
  WidgetSet: TGtk4WidgetSet;

procedure ButtonClick(Sender: TObject);
begin
  WriteLn('Hello Mini-LCL!');
end;

begin
  WidgetSet := TGtk4WidgetSet.Create;
  WidgetSet.Initialize;
  
  Form := TForm.Create(nil);
  Form.Caption := 'Mein Fenster';
  
  Button := TButton.Create(Form);
  Button.Parent := Form;
  Button.Caption := 'Klick mich';
  Button.OnClick := @ButtonClick;
  
  WidgetSet.CreateControlHandle(Form);
  WidgetSet.ShowForm(Form);
  
  WidgetSet.Run;
end.
```

## Hinweise

- **Proof-of-Concept:** Nur grundlegende Funktionalität implementiert
- **Keine Lazarus-LCL Abhängigkeit:** Komplett eigenständiges Framework
- **GTK4-spezifisch:** Andere Backends müssen separat implementiert werden
- **Minimaler Feature-Set:** Focus auf Architektur-Demo, nicht Production-Ready

## Lizenz

Dieses Mini-LCL Framework steht unter MIT-Lizenz und dient ausschließlich Demonstrationszwecken.