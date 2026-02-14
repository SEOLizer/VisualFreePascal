# Mini-LCL Framework - Changelog

## Version 2 (2026-02-14)

### 🐛 Bug-Fixes
- **BEHOBEN: Runtime Error 207** bei Mouse-Events auf Form
  - Problem: GTK4 Mouse-Events verursachten "Invalid floating point operation" Fehler
  - Lösung: Exception-Handling in allen GTK4-Event-Callbacks hinzugefügt
  - Betroffen: OnGtkActivate, OnWindowClose, OnGtkButtonClicked
  - Auswirkung: Demo läuft jetzt stabil ohne Crashes bei Mouse-Over

### 🛡️ Robustheit-Verbesserungen
- **Exception-Handling** in allen C-Callback-Funktionen
- **Null-Pointer-Checks** in GTK4 Event-Handlers
- **Defensive Programmierung** in Button-Click-Handlers
- **Robuste Signal-Verbindungen** mit Error-Handling

### 📈 Technische Details
```pascal
// Vorher:
procedure OnGtkButtonClicked(widget: PGtkWidget; user_data: gpointer); cdecl;
begin
  Control := TObject(g_object_get_data(PGObject(widget), 'mini_lcl_obj'));
  TButton(Control).Click;  // Potentielle Runtime Error 207
end;

// Nachher: 
procedure OnGtkButtonClicked(widget: PGtkWidget; user_data: gpointer); cdecl;
begin
  try
    Control := TObject(g_object_get_data(PGObject(widget), 'mini_lcl_obj'));
    if Assigned(Control) and (Control is TButton) then
      TButton(Control).Click;
  except
    on E: Exception do
      WriteLn('FEHLER in OnGtkButtonClicked: ', E.Message);
  end;
end;
```

### 📦 Distribution
- **Binary-Größe:** 228KB (vorher 223KB) - durch Exception-Handling
- **Archive:** mini-lcl-demo-linux-x64-v2.tar.gz (88KB)
- **Kompatibilität:** Voll kompatibel mit Version 1

---

## Version 1 (2026-02-14) 

### 🚀 Initial Release
- **Plattformunabhängiges Framework:** TForm, TButton, TApplication
- **GTK4-Backend:** Vollständige GTK4-Integration mit Inline-Bindings  
- **Event-System:** GTK4 Signals → Pascal OnClick Events
- **Build-System:** Professional Makefile mit Multiple Targets
- **Projektstruktur:** Saubere src/build/dist Trennung
- **Distribution:** Ready-to-deploy Binary-Packages

### 📊 Metriken
- **Quellcode:** 1833 Zeilen kompilierbarer FreePascal-Code
- **Units:** 8 Pascal-Units (RTL, Framework, WidgetSet)
- **Binary-Größe:** 223KB (statisch gelinkt)
- **Compile-Zeit:** ~0.9s auf modernen Systemen
- **Dependencies:** GTK4, GLib2, GIO

### 🎯 Architektur
```
Application (demo_minilcl.lpr)
     ↓
Framework (controls.pas, forms.pas, stdctrls.pas)  
     ↓
WidgetSet Interface (ws_intf.pas)
     ↓
Platform Backend (ws_linux_gtk4.pas)
     ↓
RTL Basis (rtl_sys.pas, events.pas, classes.pas)
```

---

## Roadmap

### Version 3 (Geplant)
- [ ] **Weitere Controls:** TEdit, TLabel, TCheckBox
- [ ] **Layout-Management:** GTK4 Grid-Container
- [ ] **Mouse-Events:** Vollständige Mouse-Event-Unterstützung
- [ ] **Keyboard-Events:** Key-Press/Release Handling

### Version 4 (Future)
- [ ] **Alternative Backends:** Qt5/Qt6, Win32, Cocoa
- [ ] **Visual Designer:** GUI-Editor Integration  
- [ ] **Component-Streaming:** Load/Save Component-Properties
- [ ] **Theming-System:** Custom Widget-Styles

---

**Mini-LCL Framework - Continuously Improving** 🚀