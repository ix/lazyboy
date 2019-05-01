;; THIS FILE WAS GENERATED WITH LAZYBOY

;; DEFINE THE ROM START LOCATION
SECTION "rom", ROM0

;; DEFINE THE START LOCATION AS 0x100
SECTION "start", ROM0[$0100]
    nop
    jp main

;; PLACE THE MAIN BLOCK AT 0x150
SECTION "main", ROM0[$0150]

main:
ld HL, $D000
ld [HL], 192
ld HL, $D001
ld [HL], 222
ld HL, $C00A
ld [HL], 250
ld HL, $C00B
ld [HL], 206
ld HL, $D000
ld DE, $C000
ld B, 10
.L1:
ld A, [HL+]
ld [DE], A
inc DE
dec B
jp nz, .L1
ld HL, $C00A
ld DE, $D00A
ld B, 10
.L2:
ld A, [HL+]
ld [DE], A
inc DE
dec B
jp nz, .L2
.L3:
jp .L3

