# Mini-LCL Framework - Installation

## Distribution-Package

Das Mini-LCL Framework kann auf zwei Arten genutzt werden:

### 1. Binary-Distribution (Endbenutzer)

**Download:**
```bash
# Lade Distribution-Archive herunter:
wget mini-lcl-demo-linux-x64.tar.gz

# Extrahiere Archiv:
tar -xzf mini-lcl-demo-linux-x64.tar.gz
cd mini-lcl-demo/
```

**System-Anforderungen:**
```bash
# Ubuntu/Debian:
sudo apt install libgtk-4-1 libglib2.0-0

# Fedora:
sudo dnf install gtk4 glib2

# Arch Linux:
sudo pacman -S gtk4 glib2
```

**Ausführung:**
```bash
./demo_minilcl
```

**Erwartetes Verhalten:**
- GTK4-Fenster öffnet sich mit Titel "Mini-LCL Demo mit GTK4"
- Button "Klick mich!" ist sichtbar und funktional
- Button-Clicks wechseln Caption zwischen "Klick mich!" und "Geklickt!"
- Fenster schließen beendet Programm sauber

---

### 2. Quellcode-Build (Entwickler)

**Repository klonen:**
```bash
git clone [repo-url] mini-lcl
cd mini-lcl/
```

**Entwicklungs-Anforderungen:**
```bash
# Ubuntu/Debian:
sudo apt install fpc libgtk-4-dev libglib2.0-dev pkg-config build-essential

# Fedora:
sudo dnf install fpc gtk4-devel glib2-devel pkg-config gcc

# Arch Linux:
sudo pacman -S fpc gtk4 glib2 pkg-config base-devel
```

**Kompilierung:**
```bash
make                    # Kompiliere Demo
make clean              # Räume Build-Artefakte auf
make dist               # Erstelle Distribution
make test               # Teste Demo (3s Timeout)
make help               # Zeige alle Targets
```

**Entwicklung:**
```bash
# Lazarus IDE (optional):
lazarus-ide mini-lcl.lpi

# Quellcode-Struktur:
src/rtl/          # Basis-RTL (rtl_sys, events, classes)
src/framework/    # GUI-Framework (controls, forms, stdctrls)  
src/widgets/      # WidgetSet-Backend (ws_intf, ws_linux_gtk4)
examples/basic/   # Demo-Programm
```

---

## Projekt-Layout

```
mini-lcl/
├── src/              # 🔧 Quellcode (für Entwickler)
├── examples/         # 📝 Beispielprogramme
├── build/            # 🏗️ Build-Artefakte (*.o, *.ppu)
├── dist/             # 📦 Distribution-Packages
├── docs/             # 📚 Dokumentation
├── tools/            # 🛠️ Build-Tools
├── Makefile          # 🔨 Build-System
├── .gitignore        # 📂 Git-Konfiguration
└── README.md         # 📖 Projekt-Info
```

**Wichtig:** 
- `src/` - Nur für Entwickler relevant
- `build/` - Temporäre Build-Artefakte  
- `dist/` - **Distribution für Endbenutzer**

---

## Troubleshooting

### Demo startet nicht
```
./demo_minilcl: error while loading shared libraries
```
**Lösung:** GTK4-Runtime installieren:
```bash
sudo apt install libgtk-4-1 libglib2.0-0
```

### Kompilierung schlägt fehl
```
Can't find unit rtl_sys used by demo_minilcl
```
**Lösung:** Verwende Makefile:
```bash
make examples  # Statt direktem fpc-Aufruf
```

### GTK4-Warnungen
```
Gtk-WARNING: Unknown key gtk-modules
```
**Lösung:** Diese Warnung ist harmlos und kann ignoriert werden.

### Fenster wird nicht angezeigt
**Lösung:** Prüfe DISPLAY-Variable:
```bash
echo $DISPLAY           # Sollte :0 oder ähnlich zeigen
export DISPLAY=:0       # Falls leer
./demo_minilcl
```

---

## Deinstallation

**Binary-Distribution:**
```bash
rm -rf mini-lcl-demo/
rm mini-lcl-demo-linux-x64.tar.gz
```

**Quellcode-Build:**
```bash
cd mini-lcl/
make distclean    # Entferne alle Build-Artefakte
cd ..
rm -rf mini-lcl/
```

---

**Mini-LCL Framework - Moderne GUI-Architektur für FreePascal** 🚀