# Deep-Linking Examples for Brick Play & Brick Contraptions

Palikkakehys supports deep-linking via URL hash fragments (`Studio.UrlPayload`), allowing external sites, educational materials, or companion apps to pass pre-placed starter models and curated kit inventories directly to **Brick Play** and **Brick Contraptions**.

---

## URL Hash Fragment Format

A Palikkakehys hash fragment carries up to two query parameters separated by `&`:

```
#model=<percent-encoded-ldraw>&parts=<part-kit-spec>
```

| Parameter | Description |
|-----------|-------------|
| `model` | Complete LDraw `.ldr` model definition (`\n` separated lines), percent-encoded. Loaded as the initial document. |
| `parts` | Comma-separated list of kit items: `<part_file>:<color_code>:<count>`. In Brick Play, defines total kit pieces (placed model pieces decrement available dock count). In Brick Contraptions, populates the "Project Parts" catalog tab. |

### LDraw Coordinate Conventions
- **System**: Right-handed Y-down (1 stud = 20 LDU, 1 brick height = 24 LDU, 1 plate = 8 LDU).
- **Contraptions Axle Line**: Technic shafts and gears sit on the common axle line at $Y = -60$ LDU.
- **Axle Matrix Along Z**: `0 0 -1 0 1 0 1 0 0` reorients an axle (default along X) to point along Z.

---

## Brick Play Examples

Production Base URL: `https://kehys.palikkaharrastajat.fi/play`  
Local Dev URL: `http://localhost:8181/`

### 1. Cozy Cottage with Window

A starter foundation with pre-built walls and an installed White 1×2×2 Window Frame (`60592.dat`). The builder's dock provides spare tan bricks to finish the walls, yellow bricks for the roof lintel, and red 45° slopes (`3039.dat`) to cap the cottage.

#### Pre-placed Starter Model (LDraw)
```ldraw
1 19 0 -24 0 1 0 0 0 1 0 0 0 1 3001.dat
1 15 0 -72 0 1 0 0 0 1 0 0 0 1 60592.dat
1 19 -30 -48 0 1 0 0 0 1 0 0 0 1 3005.dat
1 19 30 -48 0 1 0 0 0 1 0 0 0 1 3005.dat
```

#### Kit Inventory
- `3001.dat:19:3` — 3× Brick 2×4 Tan (1 placed, 2 remaining)
- `3005.dat:19:6` — 6× Brick 1×1 Tan (2 placed, 4 remaining)
- `3001.dat:14:2` — 2× Brick 2×4 Yellow (0 placed, 2 remaining)
- `3039.dat:4:4` — 4× Slope Brick 45° 2×2 Red (0 placed, 4 remaining)
- `60592.dat:15:1` — 1× Window Frame 1×2×2 White (1 placed, 0 remaining)
- `3004.dat:19:4` — 4× Brick 1×2 Tan (0 placed, 4 remaining)

#### URLs
- **Production**:  
  [`https://kehys.palikkaharrastajat.fi/play#model=1%2019%200%20-24%200%201%200%200%200%201%200%200%200%201%203001.dat%0A1%2015%200%20-72%200%201%200%200%200%201%200%200%200%201%2060592.dat%0A1%2019%20-30%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat%0A1%2019%2030%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat&parts=3001.dat%3A19%3A3%2C3005.dat%3A19%3A6%2C3001.dat%3A14%3A2%2C3039.dat%3A4%3A4%2C60592.dat%3A15%3A1%2C3004.dat%3A19%3A4`](https://kehys.palikkaharrastajat.fi/play#model=1%2019%200%20-24%200%201%200%200%200%201%200%200%200%201%203001.dat%0A1%2015%200%20-72%200%201%200%200%200%201%200%200%200%201%2060592.dat%0A1%2019%20-30%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat%0A1%2019%2030%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat&parts=3001.dat%3A19%3A3%2C3005.dat%3A19%3A6%2C3001.dat%3A14%3A2%2C3039.dat%3A4%3A4%2C60592.dat%3A15%3A1%2C3004.dat%3A19%3A4)
- **Local Dev**:  
  [`http://localhost:8181/#model=1%2019%200%20-24%200%201%200%200%200%201%200%200%200%201%203001.dat%0A1%2015%200%20-72%200%201%200%200%200%201%200%200%200%201%2060592.dat%0A1%2019%20-30%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat%0A1%2019%2030%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat&parts=3001.dat%3A19%3A3%2C3005.dat%3A19%3A6%2C3001.dat%3A14%3A2%2C3039.dat%3A4%3A4%2C60592.dat%3A15%3A1%2C3004.dat%3A19%3A4`](http://localhost:8181/#model=1%2019%200%20-24%200%201%200%200%200%201%200%200%200%201%203001.dat%0A1%2015%200%20-72%200%201%200%200%200%201%200%200%200%201%2060592.dat%0A1%2019%20-30%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat%0A1%2019%2030%20-48%200%201%200%200%200%201%200%200%200%201%203005.dat&parts=3001.dat%3A19%3A3%2C3005.dat%3A19%3A6%2C3001.dat%3A14%3A2%2C3039.dat%3A4%3A4%2C60592.dat%3A15%3A1%2C3004.dat%3A19%3A4)

---

### 2. Castle Watchtower

A 4×4-stud stone base constructed from four Light Bluish Gray 2×2 bricks (`3003.dat`). The kit contains 8 additional gray bricks to raise tower height, corner battlements (`3005.dat`), yellow lantern blocks, and red heraldic topper plates (`3024.dat`).

#### Pre-placed Starter Model (LDraw)
```ldraw
1 71 -20 -24 -20 1 0 0 0 1 0 0 0 1 3003.dat
1 71 20 -24 -20 1 0 0 0 1 0 0 0 1 3003.dat
1 71 -20 -24 20 1 0 0 0 1 0 0 0 1 3003.dat
1 71 20 -24 20 1 0 0 0 1 0 0 0 1 3003.dat
```

#### Kit Inventory
- `3003.dat:71:12` — 12× Brick 2×2 Light Bluish Gray (4 placed, 8 remaining)
- `3005.dat:71:8` — 8× Brick 1×1 Light Bluish Gray (0 placed, 8 remaining)
- `3005.dat:14:2` — 2× Brick 1×1 Yellow (0 placed, 2 remaining)
- `3024.dat:4:4` — 4× Plate 1×1 Red (0 placed, 4 remaining)

#### URLs
- **Production**:  
  [`https://kehys.palikkaharrastajat.fi/play#model=1%2071%20-20%20-24%20-20%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%2020%20-24%20-20%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%20-20%20-24%2020%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%2020%20-24%2020%201%200%200%200%201%200%200%200%201%203003.dat&parts=3003.dat%3A71%3A12%2C3005.dat%3A71%3A8%2C3005.dat%3A14%3A2%2C3024.dat%3A4%3A4`](https://kehys.palikkaharrastajat.fi/play#model=1%2071%20-20%20-24%20-20%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%2020%20-24%20-20%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%20-20%20-24%2020%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%2020%20-24%2020%201%200%200%200%201%200%200%200%201%203003.dat&parts=3003.dat%3A71%3A12%2C3005.dat%3A71%3A8%2C3005.dat%3A14%3A2%2C3024.dat%3A4%3A4)
- **Local Dev**:  
  [`http://localhost:8181/#model=1%2071%20-20%20-24%20-20%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%2020%20-24%20-20%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%20-20%20-24%2020%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%2020%20-24%2020%201%200%200%200%201%200%200%200%201%203003.dat&parts=3003.dat%3A71%3A12%2C3005.dat%3A71%3A8%2C3005.dat%3A14%3A2%2C3024.dat%3A4%3A4`](http://localhost:8181/#model=1%2071%20-20%20-24%20-20%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%2020%20-24%20-20%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%20-20%20-24%2020%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2071%2020%20-24%2020%201%200%200%200%201%200%200%200%201%203003.dat&parts=3003.dat%3A71%3A12%2C3005.dat%3A71%3A8%2C3005.dat%3A14%3A2%2C3024.dat%3A4%3A4)

---

### 3. Classic Arch Bridge

Twin stone piers set 80 LDU apart (clear span between $X = -30$ and $X = 30$). The builder bridges the gap using yellow 2×4 spans (`3001.dat`), paves the roadway with blue 1×4 deck plates (`3710.dat`), and adds red guard railings (`3004.dat`).

#### Pre-placed Starter Model (LDraw)
```ldraw
1 15 -50 -24 0 1 0 0 0 1 0 0 0 1 3003.dat
1 15 -50 -48 0 1 0 0 0 1 0 0 0 1 3003.dat
1 15 50 -24 0 1 0 0 0 1 0 0 0 1 3003.dat
1 15 50 -48 0 1 0 0 0 1 0 0 0 1 3003.dat
```

#### Kit Inventory
- `3003.dat:15:4` — 4× Brick 2×2 White (4 placed, 0 remaining)
- `3001.dat:14:3` — 3× Brick 2×4 Yellow (0 placed, 3 remaining)
- `3710.dat:1:4` — 4× Plate 1×4 Blue (0 placed, 4 remaining)
- `3004.dat:4:4` — 4× Brick 1×2 Red (0 placed, 4 remaining)

#### URLs
- **Production**:  
  [`https://kehys.palikkaharrastajat.fi/play#model=1%2015%20-50%20-24%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%20-50%20-48%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%2050%20-24%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%2050%20-48%200%201%200%200%200%201%200%200%200%201%203003.dat&parts=3003.dat%3A15%3A4%2C3001.dat%3A14%3A3%2C3710.dat%3A1%3A4%2C3004.dat%3A4%3A4`](https://kehys.palikkaharrastajat.fi/play#model=1%2015%20-50%20-24%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%20-50%20-48%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%2050%20-24%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%2050%20-48%200%201%200%200%200%201%200%200%200%201%203003.dat&parts=3003.dat%3A15%3A4%2C3001.dat%3A14%3A3%2C3710.dat%3A1%3A4%2C3004.dat%3A4%3A4)
- **Local Dev**:  
  [`http://localhost:8181/#model=1%2015%20-50%20-24%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%20-50%20-48%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%2050%20-24%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%2050%20-48%200%201%200%200%200%201%200%200%200%201%203003.dat&parts=3003.dat%3A15%3A4%2C3001.dat%3A14%3A3%2C3710.dat%3A1%3A4%2C3004.dat%3A4%3A4`](http://localhost:8181/#model=1%2015%20-50%20-24%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%20-50%20-48%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%2050%20-24%200%201%200%200%200%201%200%200%200%201%203003.dat%0A1%2015%2050%20-48%200%201%200%200%200%201%200%200%200%201%203003.dat&parts=3003.dat%3A15%3A4%2C3001.dat%3A14%3A3%2C3710.dat%3A1%3A4%2C3004.dat%3A4%3A4)

---

## Brick Contraptions Examples

Production Base URL: `https://kehys.palikkaharrastajat.fi/contraptions`  
Local Dev URL: `http://localhost:8182/`

### 1. Twin 24T Gearbox (1:1 Ratio)

A 1×7 Technic Liftarm Beam (`32524.dat`) mounted behind the axle line at $Y = -60, Z = -20$. Two Axle 4s (`3705.dat`) pass through beam holes at $X = 0$ and $X = 60$ (60 LDU distance, 3 studs). One 24T gear (`3648.dat`, pitch radius 30 LDU) is mounted on the first shaft and secured with a bushing (`6590.dat`). The builder adds the matching 24T gear to the second shaft to complete the gearbox and watch both gears turn at equal speed in opposite directions.

#### Pre-placed Starter Model (LDraw)
```ldraw
1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat
1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat
1 0 60 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat
1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3648.dat
1 7 0 -60 12 1 0 0 0 1 0 0 0 1 6590.dat
```

#### Kit Inventory (Project Parts)
- `3648.dat:7:1` — 1× 24T Spur Gear (Light Gray)
- `6590.dat:7:2` — 2× Technic Bushing (Light Gray)

#### URLs
- **Production**:  
  [`https://kehys.palikkaharrastajat.fi/contraptions#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3648.dat%3A7%3A1%2C6590.dat%3A7%3A2`](https://kehys.palikkaharrastajat.fi/contraptions#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3648.dat%3A7%3A1%2C6590.dat%3A7%3A2)
- **Local Dev**:  
  [`http://localhost:8182/#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3648.dat%3A7%3A1%2C6590.dat%3A7%3A2`](http://localhost:8182/#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3648.dat%3A7%3A1%2C6590.dat%3A7%3A2)

---

### 2. 5:1 Speed Reducer (8T Driver, 40T Driven)

An 8T driver gear (`3647.dat`, pitch radius 10 LDU) on a 1×7 beam frame meshes with a 40T driven gear (`3649.dat`, pitch radius 50 LDU). The required axle distance is $10 + 50 = 60$ LDU (3 studs). The 8T gear is pre-mounted at $X = 0$, with an empty axle at $X = 60$. Mounting the 40T gear accomplishes the machine and produces a 5:1 reduction (0.2× speed ratio, 5× torque).

#### Pre-placed Starter Model (LDraw)
```ldraw
1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat
1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat
1 0 60 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat
1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3647.dat
1 7 0 -60 12 1 0 0 0 1 0 0 0 1 6590.dat
```

#### Kit Inventory (Project Parts)
- `3649.dat:7:1` — 1× 40T Spur Gear (Light Gray)
- `6590.dat:7:2` — 2× Technic Bushing (Light Gray)

#### URLs
- **Production**:  
  [`https://kehys.palikkaharrastajat.fi/contraptions#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3649.dat%3A7%3A1%2C6590.dat%3A7%3A2`](https://kehys.palikkaharrastajat.fi/contraptions#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3649.dat%3A7%3A1%2C6590.dat%3A7%3A2)
- **Local Dev**:  
  [`http://localhost:8182/#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3649.dat%3A7%3A1%2C6590.dat%3A7%3A2`](http://localhost:8182/#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%2012%201%200%200%200%201%200%200%200%201%206590.dat&parts=3649.dat%3A7%3A1%2C6590.dat%3A7%3A2)

---

### 3. 3-Gear Train (Idler Gear Transmission)

A compound 3-gear transmission on a single 1×7 beam. The input is an 8T driver at $X = -40$ meshing with a 24T idler at $X = 0$ ($10 + 30 = 40$ LDU distance, 2 studs). A third axle sits ready at $X = 60$ ($30 + 30 = 60$ LDU distance, 3 studs). The project parts provide a 24T output gear (restoring the driver's rotational direction) and a 16T gear to experiment with alternate gear combinations.

#### Pre-placed Starter Model (LDraw)
```ldraw
1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat
1 0 -40 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat
1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat
1 0 60 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat
1 7 -40 -60 0 1 0 0 0 1 0 0 0 1 3647.dat
1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3648.dat
```

#### Kit Inventory (Project Parts)
- `3648.dat:7:1` — 1× 24T Spur Gear (Light Gray)
- `4019.dat:7:1` — 1× 16T Spur Gear (Light Gray)
- `6590.dat:7:3` — 3× Technic Bushing (Light Gray)

#### URLs
- **Production**:  
  [`https://kehys.palikkaharrastajat.fi/contraptions#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%20-40%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%20-40%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat&parts=3648.dat%3A7%3A1%2C4019.dat%3A7%3A1%2C6590.dat%3A7%3A3`](https://kehys.palikkaharrastajat.fi/contraptions#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%20-40%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%20-40%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat&parts=3648.dat%3A7%3A1%2C4019.dat%3A7%3A1%2C6590.dat%3A7%3A3)
- **Local Dev**:  
  [`http://localhost:8182/#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%20-40%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%20-40%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat&parts=3648.dat%3A7%3A1%2C4019.dat%3A7%3A1%2C6590.dat%3A7%3A3`](http://localhost:8182/#model=1%201%200%20-60%20-20%200%200%20-1%200%201%200%201%200%200%2032524.dat%0A1%200%20-40%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%200%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%200%2060%20-60%200%200%200%20-1%200%201%200%201%200%200%203705.dat%0A1%207%20-40%20-60%200%201%200%200%200%201%200%200%200%201%203647.dat%0A1%207%200%20-60%200%201%200%200%200%201%200%200%200%201%203648.dat&parts=3648.dat%3A7%3A1%2C4019.dat%3A7%3A1%2C6590.dat%3A7%3A3)
