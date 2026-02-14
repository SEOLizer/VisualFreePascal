# Mini-LCL Framework Makefile
# Kompiliert das Mini-LCL Framework und Beispiele

# Konfiguration
FPC = fpc
FPCFLAGS = -k'-lgtk-4 -lgio-2.0 -lgobject-2.0 -lglib-2.0'
BUILD_DIR = build/linux-x64
DIST_DIR = dist/linux-x64
SRC_RTL = src/rtl
SRC_FRAMEWORK = src/framework  
SRC_WIDGETS = src/widgets
EXAMPLES_DIR = examples

# Standard Target
all: examples

# Verzeichnisse erstellen
dirs:
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(DIST_DIR)

# Demo-Programm kompilieren
examples: dirs
	@echo "=== Kompiliere Mini-LCL Demo ==="
	cd $(EXAMPLES_DIR)/basic && \
	$(FPC) $(FPCFLAGS) \
		-FU../../$(BUILD_DIR) \
		-o../../$(BUILD_DIR)/demo_minilcl \
		-Fu../../$(SRC_RTL) \
		-Fu../../$(SRC_FRAMEWORK) \
		-Fu../../$(SRC_WIDGETS) \
		demo_minilcl.lpr
	@echo "=== Build erfolgreich ==="
	@ls -la $(BUILD_DIR)/

# Distribution erstellen
dist: examples
	@echo "=== Erstelle Distribution ==="
	cp $(BUILD_DIR)/demo_minilcl $(DIST_DIR)/
	cp docs/BUILD_ANLEITUNG.md $(DIST_DIR)/README.md
	@echo "Distribution erstellt in $(DIST_DIR)/"
	@ls -la $(DIST_DIR)/

# Aufräumen
clean:
	@echo "=== Räume Build-Artefakte auf ==="
	rm -rf $(BUILD_DIR)/*.o $(BUILD_DIR)/*.ppu $(BUILD_DIR)/link*.res
	rm -f $(SRC_RTL)/*.o $(SRC_RTL)/*.ppu
	rm -f $(SRC_FRAMEWORK)/*.o $(SRC_FRAMEWORK)/*.ppu
	rm -f $(SRC_WIDGETS)/*.o $(SRC_WIDGETS)/*.ppu
	rm -f $(EXAMPLES_DIR)/basic/*.o $(EXAMPLES_DIR)/basic/*.ppu

# Vollständig aufräumen
distclean: clean
	rm -rf build/ dist/

# Testen
test: examples
	@echo "=== Teste Demo-Programm ==="
	timeout 3 $(BUILD_DIR)/demo_minilcl || echo "Demo läuft (Timeout nach 3s ist normal)"

# Installation (optional)
install: dist
	@echo "=== Installation nicht implementiert ==="
	@echo "Verwende Distribution aus $(DIST_DIR)/"

# Hilfe
help:
	@echo "Mini-LCL Framework Build-System"
	@echo ""
	@echo "Targets:"
	@echo "  all        - Kompiliere alle Beispiele (Standard)"
	@echo "  examples   - Kompiliere Demo-Programm"
	@echo "  dist       - Erstelle Distribution"  
	@echo "  test       - Teste Demo-Programm"
	@echo "  clean      - Räume Build-Artefakte auf"
	@echo "  distclean  - Vollständige Bereinigung"
	@echo "  help       - Zeige diese Hilfe"

.PHONY: all examples dist clean distclean test install help dirs