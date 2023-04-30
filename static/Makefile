SOURCE = static
BUILD = build
ICONS = $(wildcard $(SOURCE)/icons/*)
FAVICON_RESOLUTIONS = 32x32 192x192 512x512

BUILT_FAVICONS = $(patsubst %, $(BUILD)/favicon-%.png, $(FAVICON_RESOLUTIONS))
BUILT_ICONS = $(patsubst $(SOURCE)%, $(BUILD)%, $(ICONS))
BUILT_FILES =

.PHONY: all
all: $(BUILD)/favicon.ico $(BUILD)/favicon.png $(BUILT_FAVICONS) $(BUILD)/fonts.css $(BUILT_ICONS) $(BUILT_FILES)

$(BUILD)/favicon.ico: $(BUILD)/favicon.png $(BUILD)
	cp "$<" "$@"

$(BUILD)/favicon.png: $(BUILD)/favicon-32x32.png $(BUILD)
	cp "$<" "$@"

$(BUILT_FAVICONS): $(BUILD)/favicon-%.png: $(SOURCE)/favicon.xpm $(BUILD)
	convert "$<" -scale "$*" "$@"

$(BUILD)/fonts.css: $(SOURCE)/fonts.css $(BUILD)
	cp "$<" "$@"

$(BUILT_ICONS): $(BUILD)/icons/%: $(SOURCE)/icons/% $(BUILD)/icons
	convert "$<" -resize "128x128" "$@"

$(BUILT_FILES): $(BUILD)/%: $(SOURCE)/% $(BUILD)
	cp "$<" "$@"

$(BUILD)/icons:
	mkdir --parents "$@"

$(BUILD):
	mkdir --parents "$@"

.PHONY: clean
clean:
	rm --force --recursive $(BUILD)
