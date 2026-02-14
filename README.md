# Mini-LCL Framework

**Ein minimales, plattformunabhängiges GUI-Framework für FreePascal mit GTK4-Backend**

## Überblick

Mini-LCL ist ein Proof-of-Concept für eine saubere, erweiterbare GUI-Architektur ohne Lazarus-LCL-Abhängigkeiten. Das Framework demonstriert moderne Design-Patterns wie WidgetSet-Abstraktion, Event-Bridge-System und plattformunabhängige Control-Hierarchie.

## Features

- ✅ **Plattformunabhängige API:** TForm, TButton, TApplication
- ✅ **WidgetSet-Pattern:** Saubere Backend-Abstraktion  
- ✅ **GTK4-Integration:** Vollständige GTK4-Unterstützung mit Inline-Bindings
- ✅ **Event-System:** GTK4 Signals → Pascal OnClick Events
- ✅ **DFM-Streaming:** Vollständiger DFM/LFM Parser und Writer
  - Parse DFM-Dateien (Text-Format)
  - AST-basierte Manipulation
  - Roundtrip-Stabilität (Parse → Modify → Serialize)
  - Unterstützt: Objekthierarchien, Properties, Sets, Collections
- ✅ **Production-Ready:** Persistente Application mit sauberem Shutdown

## Schnellstart

### Voraussetzungen
```bash
# Ubuntu/Debian:
sudo apt install fpc libgtk-4-dev libglib2.0-dev pkg-config

# Fedora:  
sudo dnf install fpc gtk4-devel glib2-devel pkg-config
```

### Kompilierung
```bash
git clone [repo-url] mini-lcl
cd mini-lcl
make
```

### Demo ausführen
```bash
# Demo starten:
make test

# Oder direkt:
./build/linux-x64/demo_minilcl
```

## Projektstruktur

```
mini-lcl/
├── src/                    # Quellcode
│   ├── rtl/               # RTL-Basis (Exceptions, Events, Components)
│   ├── framework/         # GUI-Framework (Controls, Forms, StdCtrls)
│   ├── widgets/           # WidgetSet-Backend (Interface + GTK4)
│   └── streaming/         # DFM/LFM Parser und Writer
├── examples/              # Beispielprogramme
│   └── basic/            # Basis-Demo
├── build/                # Build-Artefakte
│   └── linux-x64/       # Platform-spezifische Builds
├── dist/                 # Distribution-Packages
├── docs/                 # Dokumentation
└── tools/                # Build-Tools und Utilities
```

## Architektur

### Schichtenmodell
```
┌─────────────────┐
│  Application    │  ← demo_minilcl.lpr
├─────────────────┤
│  Framework      │  ← controls.pas, forms.pas, stdctrls.pas
├─────────────────┤
│  WidgetSet      │  ← ws_intf.pas  
├─────────────────┤
│  Platform       │  ← ws_linux_gtk4.pas
├─────────────────┤
│  Streaming      │  ← dfm_parser.pas, dfm_writer.pas
├─────────────────┤
│  RTL            │  ← rtl_sys.pas, events.pas, classes.pas  
└─────────────────┘
```

## DFM/LFM Streaming

Das Framework enthält einen vollständigen **DFM-Parser und Writer** für Delphi Form Files:

### Features
- **Parsen**: Lade DFM-Dateien aus Textformat
- **AST-Manipulation**: Bearbeite Objekthierarchien programmatisch  
- **Serialisierung**: Speichere zurück als DFM-Text
- **Roundtrip**: Parse → Modify → Serialize ohne Datenverlust

### Unterstützte Value-Typen
| Typ | Beispiel |
|-----|----------|
| Integer | `Left = 100` |
| Float | `Opacity = 0.5` |
| Boolean | `Visible = True` |
| String | `Caption = 'Hello'` |
| Identifier | `BorderStyle = bsDialog` |
| Set | `Anchors = [akLeft, akTop]` |
| Collection | `Items = < item ... end >` |
| Binary | `{ 4D5A... }` |

### Beispiel
```pascal
uses dfm_ast, dfm_parser, dfm_writer;

// DFM laden
var Parser := TDfmParser.Create;
var Doc := Parser.ParseText('object Form1: TForm ... end');

// Manipulieren
Doc.Root.Properties.AddObject('Left', TDfmValue.Create(dvkInteger, '200'));

// Speichern
var Writer := TDfmWriter.Create;
var Output := Writer.WriteDocument(Doc);
```

### Design-Patterns
- **Factory Pattern:** TWidgetSet.CreateControlHandle()
- **Bridge Pattern:** Plattformunabhängige Controls ↔ Platform Widgets
- **Observer Pattern:** GTK4 Signals → Pascal Events
- **Template Method:** TControl-Hierarchie mit virtuellen Methoden

## Entwicklung

### Neue Controls hinzufügen
1. **Framework:** Definiere TMyControl in src/framework/
2. **Backend:** Implementiere TGtk4WSMyControl in src/widgets/
3. **Factory:** Erweitere CreateControlHandle für neuen Control-Typ

### Neue Platforms hinzufügen  
1. **Backend:** Erstelle src/widgets/ws_platform_backend.pas
2. **Implementation:** Implementiere TWidgetSet-Interface
3. **Demo:** Wechsle WidgetSet-Implementation

## Build-Targets

```bash
make all        # Kompiliere alles
make examples   # Kompiliere Demo  
make dist       # Erstelle Distribution
make clean      # Räume Build-Artefakte auf
make test       # Teste Demo-Programm
make help       # Zeige alle Targets
```

## Lizenz

MIT License - Siehe docs/ für Details.

## Beiträge

Pull Requests willkommen! Siehe CONTRIBUTING.md für Guidelines.

---

**Mini-LCL Framework - Moderne GUI-Architektur für FreePascal** 🚀