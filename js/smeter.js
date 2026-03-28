// @ts-check

(() => {
  const SVG_NS = "http://www.w3.org/2000/svg";
  const svg = document.getElementById("signal-meter-svg");

  if (!(svg instanceof SVGSVGElement)) {
    return;
  }

  const config = {
    pathD: "M 100 200 Q 300 150 500 200",
    meterLabelTrack: -15,
    meterBarTrack: 15,
    wattTrack: 45,
    filterTrack: 70,
    swrBarTrack: 90,
    swrLabelTrack: 120,
    barSize: { width: 8, height: 18 },
    swrBarSize: { width: 10, height: 10 },
    swrBarCount: 30,
    sTangentOffset: -60,
    unitTangentOffset: 60,
  };

  const meterScale = [
    { text: "", barsToNext: 2 },
    { text: "1", barsToNext: 3 },
    { text: "3", barsToNext: 3 },
    { text: "5", barsToNext: 3 },
    { text: "7", barsToNext: 3 },
    { text: "9", barsToNext: 5 },
    { text: "20", barsToNext: 5 },
    { text: "40", barsToNext: 5 },
    { text: "60", barsToNext: 0 },
  ];

  const model = buildMeterModel(svg, config, meterScale);
  const state = createInitialState(model);
  const updateIntervalMs = window.matchMedia("(prefers-reduced-motion: reduce)").matches
    ? 3200
    : 1600;

  mountMeter(model, state);
  updateMeter(model, state);
  window.setInterval(() => {
    stepMeter(model, state);
    updateMeter(model, state);
  }, updateIntervalMs);

  /**
   * @param {SVGSVGElement} svg
   * @param {{
   *   pathD: string,
   *   meterLabelTrack: number,
   *   meterBarTrack: number,
   *   wattTrack: number,
   *   filterTrack: number,
   *   swrBarTrack: number,
   *   swrLabelTrack: number,
   *   barSize: { width: number, height: number },
   *   swrBarSize: { width: number, height: number },
   *   swrBarCount: number,
   *   sTangentOffset: number,
   *   unitTangentOffset: number,
   * }} config
   * @param {{ text: string, barsToNext: number }[]} meterScale
   */
  function buildMeterModel(svg, config, meterScale) {
    const guidePath = createGuidePath(config.pathD);
    const pathLength = guidePath.getTotalLength();
    const meterStops = layoutStops(meterScale);
    const meterMarkers = visibleMarkers(meterStops);
    const scaleStartPos = leadingBarPos(meterStops);
    const scaleMidPos = posForLabel(meterMarkers, "9");
    const scaleEndPos = meterStops[meterStops.length - 1].pos;
    const meterBars = buildMeterBars(meterStops, scaleMidPos);
    const wattMarkers = buildValueMarkers(
      [0, 10, 25, 50, 100],
      scaleStartPos,
      50,
      scaleMidPos,
    );
    const filterRow = buildSeparatorTrack(
      "FILTER",
      scaleStartPos,
      scaleMidPos,
      scaleEndPos,
    );
    const swrBars = buildEvenBoxes(
      config.swrBarCount,
      scaleStartPos,
      scaleEndPos,
    );
    const swrMarkers = buildSWRMarkers(scaleStartPos, scaleEndPos);
    const initialSignalLevel = barIndexForPos(
      meterBars,
      posForLabel(meterMarkers, "9"),
    );
    const minSignalLevel = barIndexForPos(
      meterBars,
      posForLabel(meterMarkers, "7"),
    );
    const maxWanderSignalLevel = barIndexForPos(
      meterBars,
      posForLabel(meterMarkers, "40"),
    );

    return {
      svg,
      config,
      guidePath,
      pathLength,
      meterStops,
      meterMarkers,
      scaleStartPos,
      scaleMidPos,
      scaleEndPos,
      meterBars,
      wattMarkers,
      filterRow,
      swrBars,
      swrMarkers,
      initialSignalLevel,
      minSignalLevel,
      maxSignalLevel: meterBars.length,
      maxWanderSignalLevel,
      meterBarNodes: /** @type {SVGRectElement[]} */ ([]),
      swrBarNodes: /** @type {SVGRectElement[]} */ ([]),
    };
  }

  /**
   * @param {{
   *   initialSignalLevel: number,
   * }} model
   */
  function createInitialState(model) {
    return {
      level: model.initialSignalLevel,
      swrLevel: 4.8,
      nextDirection: randomDirection(),
    };
  }

  /**
   * @param {{
   *   svg: SVGSVGElement,
   *   meterBars: { pos: number, isOver?: boolean }[],
   *   swrBars: { pos: number }[],
   * }} model
   * @param {{
   *   level: number,
   *   swrLevel: number,
   * }} state
   */
  function mountMeter(model, state) {
    model.svg.innerHTML = meterMarkup(model, state.level, state.swrLevel);
    model.meterBarNodes = collectBarNodes(
      model.svg,
      ".signal-meter-bars rect",
      model.meterBars.length,
    );
    model.swrBarNodes = collectBarNodes(
      model.svg,
      ".signal-swr-bars rect",
      model.swrBars.length,
    );
  }

  /**
   * @param {{
   *   minSignalLevel: number,
   *   maxWanderSignalLevel: number,
   *   maxSignalLevel: number,
   * }} model
   * @param {{
   *   level: number,
   *   swrLevel: number,
   *   nextDirection: number,
   * }} state
   */
  function stepMeter(model, state) {
    state.level = clamp(
      state.level + (state.nextDirection * pickStepSize()),
      model.minSignalLevel,
      model.maxWanderSignalLevel,
    );
    state.nextDirection = randomDirection();

    const swrTarget = 3.8 + ((state.level / model.maxSignalLevel) * 2);
    state.swrLevel = clamp(
      lerp(state.swrLevel, swrTarget, 0.18),
      2.8,
      8.2,
    );
  }

  /**
   * @param {{
   *   meterBarNodes: SVGRectElement[],
   *   meterBars: { isOver?: boolean }[],
   *   swrBarNodes: SVGRectElement[],
   *   swrBars: { pos: number }[],
   *   maxWanderSignalLevel: number,
   * }} model
   * @param {{
   *   level: number,
   *   swrLevel: number,
   *   nextDirection: number,
   * }} state
   */
  function updateMeter(model, state) {
    const previewSignalLevel = state.nextDirection > 0
      ? Math.min(state.level + 0.5, model.maxWanderSignalLevel)
      : state.level;

    updateBarNodes(model.meterBarNodes, model.meterBars, previewSignalLevel);
    updateBarNodes(model.swrBarNodes, model.swrBars, state.swrLevel);
  }

  /**
   * @param {{
   *   config: {
   *     meterLabelTrack: number,
   *     meterBarTrack: number,
   *     wattTrack: number,
   *     filterTrack: number,
   *     swrBarTrack: number,
   *     swrLabelTrack: number,
   *     barSize: { width: number, height: number },
   *     swrBarSize: { width: number, height: number },
   *     sTangentOffset: number,
   *     unitTangentOffset: number,
   *   },
   *   guidePath: SVGPathElement,
   *   pathLength: number,
   *   meterStops: { text: string, barsToNext: number, pos: number }[],
   *   meterMarkers: { text: string, pos: number }[],
   *   scaleStartPos: number,
   *   scaleMidPos: number,
   *   scaleEndPos: number,
   *   meterBars: { pos: number, isOver?: boolean }[],
   *   wattMarkers: { text: string, pos: number }[],
   *   filterRow: {
   *     labels: { text: string, pos: number }[],
   *     segments: { text: string, pos: number }[],
   *   },
   *   swrBars: { pos: number }[],
   *   swrMarkers: { text: string, pos: number }[],
   * }} model
   * @param {number} signalLevel
   * @param {number} swrLevel
   * @returns {string}
   */
  function meterMarkup(model, signalLevel, swrLevel) {
    const sampleAt = (pos) => sampleArc(model.guidePath, model.pathLength, pos);
    const pointAt = (pos, trackOffset) => pointOnTrack(sampleAt(pos), trackOffset);
    const sideLabelAt = (pos, trackOffset, tangentOffset) =>
      tangentOffsetPoint(sampleAt(pos), trackOffset, tangentOffset);

    const renderCurveSegment = (
      startPos,
      endPos,
      trackOffset,
      startText,
      endText,
      className,
    ) => {
      const trimmedStart = startText
        ? startPos + textGapPos(startText, model.pathLength)
        : startPos;
      const trimmedEnd = endText
        ? endPos - textGapPos(endText, model.pathLength)
        : endPos;

      return renderPath(
        className,
        trackSegmentPath(trimmedStart, trimmedEnd, (pos) => pointAt(pos, trackOffset)),
      );
    };

    const renderTrackSegments = (markers, trackOffset, segmentClass, startOverride) => {
      const nodes = [];

      for (let index = 0; index < markers.length - 1; index += 1) {
        const start = markers[index];
        const end = markers[index + 1];
        const startPos = index === 0 && startOverride
          ? startOverride(start, end)
          : start.pos;

        nodes.push(
          renderCurveSegment(
            startPos,
            end.pos,
            trackOffset,
            start.text,
            end.text,
            segmentClass ? segmentClass(start, end) : "connector",
          ),
        );
      }

      return nodes;
    };

    const renderTrackLabels = (markers, trackOffset, labelClass, rotateLabels) => {
      return markers.map((marker) => {
        const sample = sampleAt(marker.pos);
        return renderText(
          labelClass ? labelClass(marker) : "label",
          pointOnTrack(sample, trackOffset),
          marker.text,
          rotateLabels ? sample : undefined,
        );
      });
    };

    const renderTrack = (labels, segments, trackOffset, options = {}) => {
      const nodes = [
        ...renderTrackSegments(
          segments,
          trackOffset,
          options.segmentClass,
          options.startOverride,
        ),
        ...renderTrackLabels(
          labels,
          trackOffset,
          options.labelClass,
          options.rotateLabels !== false,
        ),
      ];

      if (
        options.unitText &&
        options.unitPos !== undefined &&
        options.unitTangentOffset !== undefined
      ) {
        nodes.push(
          renderText(
            "label unit",
            tangentOffsetPoint(
              sampleAt(options.unitPos),
              trackOffset,
              options.unitTangentOffset,
            ),
            options.unitText,
            sampleAt(options.unitPos),
          ),
        );
      }

      return nodes;
    };

    const renderBars = (boxes, trackOffset, size, litLevel) => {
      return boxes.map((box, index) => {
        const sample = sampleAt(box.pos);
        return renderRect(
          barClass(box, index, litLevel),
          pointOnTrack(sample, trackOffset),
          size,
          rectRotation(sample.normal),
        );
      });
    };

    const meterLabelClass = (marker) =>
      marker.pos > model.scaleMidPos ? "label label-over" : "label";

    const meterSegmentClass = (_start, end) =>
      end.pos > model.scaleMidPos ? "connector connector-over" : "connector";

    return [
      ...renderTrack(
        model.meterMarkers,
        model.meterStops,
        model.config.meterLabelTrack,
        {
          unitText: "dB",
          unitPos: posForLabel(model.meterMarkers, "60"),
          unitTangentOffset: model.config.unitTangentOffset,
          labelClass: meterLabelClass,
          segmentClass: meterSegmentClass,
          startOverride: (start) =>
            leadingMeterStartPos(model.meterStops, model.config.barSize.width, model.pathLength),
        },
      ),
      renderGroup(
        "signal-meter-bars",
        renderBars(
          model.meterBars,
          model.config.meterBarTrack,
          model.config.barSize,
          signalLevel,
        ),
      ),
      renderText(
        "label",
        sideLabelAt(
          posForLabel(model.meterMarkers, "1"),
          model.config.meterLabelTrack,
          model.config.sTangentOffset,
        ),
        "S",
        sampleAt(posForLabel(model.meterMarkers, "1")),
      ),
      ...renderTrack(
        model.wattMarkers,
        model.wattMarkers,
        model.config.wattTrack,
        {
          unitText: "W",
          unitPos: model.wattMarkers[model.wattMarkers.length - 1].pos,
          unitTangentOffset: model.config.unitTangentOffset,
        },
      ),
      ...renderTrack(
        model.filterRow.labels,
        model.filterRow.segments,
        model.config.filterTrack,
        { rotateLabels: false },
      ),
      renderGroup(
        "signal-swr-bars",
        renderBars(
          model.swrBars,
          model.config.swrBarTrack,
          model.config.swrBarSize,
          swrLevel,
        ),
      ),
      renderText(
        "label",
        sideLabelAt(
          model.scaleStartPos,
          model.config.swrBarTrack,
          model.config.sTangentOffset,
        ),
        "SWR",
        sampleAt(model.scaleStartPos),
      ),
      ...renderTrackLabels(
        model.swrMarkers,
        model.config.swrLabelTrack,
        undefined,
        true,
      ),
    ].join("\n");
  }

  /**
   * @param {{ text: string, barsToNext: number }[]} specs
   * @returns {{ text: string, barsToNext: number, pos: number }[]}
   */
  function layoutStops(specs) {
    let totalUnits = 0;

    for (const spec of specs.slice(0, -1)) {
      totalUnits += spec.barsToNext + 1;
    }

    let usedUnits = 0;

    return specs.map((spec, index) => {
      if (index > 0) {
        const prev = specs[index - 1];
        usedUnits += prev.barsToNext + 1;
      }

      return {
        ...spec,
        pos: totalUnits === 0 ? 0 : usedUnits / totalUnits,
      };
    });
  }

  /**
   * @param {{ text: string, pos: number }[]} stops
   * @returns {{ text: string, pos: number }[]}
   */
  function visibleMarkers(stops) {
    return stops
      .filter((stop) => stop.text !== "")
      .map((stop) => ({ text: stop.text, pos: stop.pos }));
  }

  /**
   * @param {number[]} values
   * @param {number} startPos
   * @param {number} anchorValue
   * @param {number} anchorPos
   * @returns {{ text: string, pos: number }[]}
   */
  function buildValueMarkers(values, startPos, anchorValue, anchorPos) {
    const posPerUnit = (anchorPos - startPos) / anchorValue;

    return values.map((value) => ({
      text: String(value),
      pos: startPos + (value * posPerUnit),
    }));
  }

  /**
   * @param {string} text
   * @param {number} startPos
   * @param {number} labelPos
   * @param {number} endPos
   * @returns {{
   *   labels: { text: string, pos: number }[],
   *   segments: { text: string, pos: number }[],
   * }}
   */
  function buildSeparatorTrack(text, startPos, labelPos, endPos) {
    const label = { text, pos: labelPos };

    return {
      labels: [label],
      segments: [
        { text: "", pos: startPos },
        label,
        { text: "", pos: endPos },
      ],
    };
  }

  /**
   * @param {{ text: string, barsToNext: number, pos: number }[]} stops
   * @param {number} overStartPos
   * @returns {{ pos: number, isOver?: boolean }[]}
   */
  function buildMeterBars(stops, overStartPos) {
    const boxes = [];

    for (let index = 0; index < stops.length - 1; index += 1) {
      const start = stops[index];
      const end = stops[index + 1];

      for (let step = 1; step <= start.barsToNext; step += 1) {
        const pos = lerp(start.pos, end.pos, step / (start.barsToNext + 1));
        boxes.push({ pos, isOver: pos > overStartPos });
      }

      if (end.text !== "") {
        boxes.push({
          pos: end.pos,
          isOver: end.pos > overStartPos,
        });
      }
    }

    return boxes;
  }

  /**
   * @param {number} count
   * @param {number} startPos
   * @param {number} endPos
   * @returns {{ pos: number }[]}
   */
  function buildEvenBoxes(count, startPos, endPos) {
    if (count <= 1) {
      return [{ pos: startPos }];
    }

    return Array.from({ length: count }, (_, index) => ({
      pos: lerp(startPos, endPos, index / (count - 1)),
    }));
  }

  /**
   * @param {number} startPos
   * @param {number} endPos
   * @returns {{ text: string, pos: number }[]}
   */
  function buildSWRMarkers(startPos, endPos) {
    const specs = [
      { text: "1", value: 1 },
      { text: "1.5", value: 1.5 },
      { text: "2", value: 2 },
      { text: "3", value: 3 },
      { text: "∞", value: Infinity },
    ];

    return specs.map((spec) => ({
      text: spec.text,
      pos: swrPos(spec.value, startPos, endPos),
    }));
  }

  /**
   * @param {number} value
   * @param {number} startPos
   * @param {number} endPos
   * @returns {number}
   */
  function swrPos(value, startPos, endPos) {
    const amount = Number.isFinite(value)
      ? (value - 1) / (value + 1)
      : 1;

    return lerp(startPos, endPos, amount);
  }

  /**
   * @param {SVGRectElement[]} nodes
   * @param {{ isOver?: boolean }[]} boxes
   * @param {number} litLevel
   */
  function updateBarNodes(nodes, boxes, litLevel) {
    nodes.forEach((node, index) => {
      node.setAttribute("class", barClass(boxes[index], index, litLevel));
    });
  }

  /**
   * @param {SVGSVGElement} root
   * @param {string} selector
   * @param {number} expectedCount
   * @returns {SVGRectElement[]}
   */
  function collectBarNodes(root, selector, expectedCount) {
    const nodes = Array.from(root.querySelectorAll(selector));

    if (nodes.length !== expectedCount || nodes.some((node) => !(node instanceof SVGRectElement))) {
      throw new Error(`Expected ${expectedCount} rects for ${selector}.`);
    }

    return /** @type {SVGRectElement[]} */ (nodes);
  }

  /**
   * @param {{ isOver?: boolean }} box
   * @param {number} index
   * @param {number} litLevel
   * @returns {string}
   */
  function barClass(box, index, litLevel) {
    const classes = ["bar"];
    const state = lightState(litLevel, index);

    if (state === "full") {
      classes.push("bar-lit");
    } else if (state === "half") {
      classes.push("bar-half");
    }

    if (box.isOver && state !== "off") {
      classes.push("bar-over");
    }

    return classes.join(" ");
  }

  /**
   * @param {number} litLevel
   * @param {number} index
   * @returns {"off" | "half" | "full"}
   */
  function lightState(litLevel, index) {
    const level = Math.max(0, litLevel);

    if (level >= index + 1) {
      return "full";
    }

    if (level > index) {
      return "half";
    }

    return "off";
  }

  /**
   * @param {string} className
   * @param {string} d
   * @returns {string}
   */
  function renderPath(className, d) {
    return [
      `<path`,
      `  class="${className}"`,
      `  d="${d}"`,
      `  fill="none"`,
      `/>`,
    ].join("\n");
  }

  /**
   * @param {string} className
   * @param {{ x: number, y: number }} point
   * @param {string} content
   * @param {{ tangent: { x: number, y: number } } | undefined} sample
   * @returns {string}
   */
  function renderText(className, point, content, sample) {
    const transform = sample
      ? `rotate(${fmt(textRotation(sample.tangent))} ${fmt(point.x)} ${fmt(point.y)})`
      : undefined;

    return [
      `<text`,
      `  class="${className}"`,
      `  x="${fmt(point.x)}"`,
      `  y="${fmt(point.y)}"`,
      ...(transform ? [`  transform="${transform}"`] : []),
      `>${content}</text>`,
    ].join("\n");
  }

  /**
   * @param {string} className
   * @param {{ x: number, y: number }} center
   * @param {{ width: number, height: number }} size
   * @param {number} rotation
   * @returns {string}
   */
  function renderRect(className, center, size, rotation) {
    const x = center.x - (size.width / 2);
    const y = center.y - (size.height / 2);
    const transform = [
      `rotate(${fmt(rotation)}`,
      `${fmt(center.x)}`,
      `${fmt(center.y)})`,
    ].join(" ");

    return [
      `<rect`,
      `  class="${className}"`,
      `  x="${fmt(x)}"`,
      `  y="${fmt(y)}"`,
      `  width="${size.width}"`,
      `  height="${size.height}"`,
      `  transform="${transform}"`,
      `/>`,
    ].join("\n");
  }

  /**
   * @param {string} className
   * @param {string[]} nodes
   * @returns {string}
   */
  function renderGroup(className, nodes) {
    return [
      `<g class="${className}">`,
      ...nodes,
      `</g>`,
    ].join("\n");
  }

  /**
   * @param {number} startPos
   * @param {number} endPos
   * @param {(pos: number) => { x: number, y: number }} pointAt
   * @returns {string}
   */
  function trackSegmentPath(startPos, endPos, pointAt) {
    const span = Math.max(0, endPos - startPos);
    const steps = Math.max(4, Math.ceil(span * 40));
    const commands = [];

    for (let step = 0; step <= steps; step += 1) {
      const point = pointAt(lerp(startPos, endPos, step / steps));
      commands.push(`${step === 0 ? "M" : "L"} ${fmt(point.x)} ${fmt(point.y)}`);
    }

    return commands.join(" ");
  }

  /**
   * @param {{ base: { x: number, y: number }, normal: { x: number, y: number } }} sample
   * @param {number} trackOffset
   * @returns {{ x: number, y: number }}
   */
  function pointOnTrack(sample, trackOffset) {
    return add(sample.base, scale(sample.normal, trackOffset));
  }

  /**
   * @param {{ base: { x: number, y: number }, tangent: { x: number, y: number }, normal: { x: number, y: number } }} sample
   * @param {number} trackOffset
   * @param {number} tangentOffset
   * @returns {{ x: number, y: number }}
   */
  function tangentOffsetPoint(sample, trackOffset, tangentOffset) {
    return add(
      pointOnTrack(sample, trackOffset),
      scale(sample.tangent, tangentOffset),
    );
  }

  /**
   * @param {{ text: string, barsToNext: number, pos: number }[]} meterStops
   * @returns {number}
   */
  function leadingBarPos(meterStops) {
    const start = meterStops[0];
    const end = meterStops[1];
    return lerp(start.pos, end.pos, 1 / (start.barsToNext + 1));
  }

  /**
   * @param {{ text: string, barsToNext: number, pos: number }[]} meterStops
   * @param {number} barWidth
   * @param {number} pathLength
   * @returns {number}
   */
  function leadingMeterStartPos(meterStops, barWidth, pathLength) {
    return leadingBarPos(meterStops) - ((barWidth / 2) / pathLength);
  }

  /**
   * @param {{ text: string, pos: number }[]} markers
   * @param {string} label
   * @returns {number}
   */
  function posForLabel(markers, label) {
    const marker = markers.find((item) => item.text === label);

    if (!marker) {
      throw new Error(`Unknown meter label: ${label}`);
    }

    return marker.pos;
  }

  /**
   * @param {{ pos: number }[]} bars
   * @param {number} pos
   * @returns {number}
   */
  function barIndexForPos(bars, pos) {
    let bestIndex = 0;
    let bestDistance = Infinity;

    bars.forEach((bar, index) => {
      const distance = Math.abs(bar.pos - pos);

      if (distance < bestDistance) {
        bestIndex = index;
        bestDistance = distance;
      }
    });

    return bestIndex;
  }

  /**
   * @param {SVGPathElement} guidePath
   * @param {number} pathLength
   * @param {number} pos
   * @returns {{
   *   pos: number,
   *   base: { x: number, y: number },
   *   tangent: { x: number, y: number },
   *   normal: { x: number, y: number },
   * }}
   */
  function sampleArc(guidePath, pathLength, pos) {
    const length = pathLength * pos;
    const delta = 0.5;
    const base = pathPoint(guidePath, length);
    const before = pathPoint(guidePath, clamp(length - delta, 0, pathLength));
    const after = pathPoint(guidePath, clamp(length + delta, 0, pathLength));
    const tangent = unit(sub(after, before));
    const normal = outwardNormal(tangent);

    return { pos, base, tangent, normal };
  }

  /**
   * @param {SVGPathElement} guidePath
   * @param {number} length
   * @returns {{ x: number, y: number }}
   */
  function pathPoint(guidePath, length) {
    const point = guidePath.getPointAtLength(length);
    return { x: point.x, y: point.y };
  }

  /**
   * @param {string} d
   * @returns {SVGPathElement}
   */
  function createGuidePath(d) {
    const path = document.createElementNS(SVG_NS, "path");
    path.setAttribute("d", d);
    path.setAttribute("fill", "none");
    path.setAttribute("stroke", "none");
    return path;
  }

  /**
   * @returns {number}
   */
  function randomDirection() {
    return Math.random() < 0.5 ? -1 : 1;
  }

  /**
   * @returns {number}
   */
  function pickStepSize() {
    return Math.random() < 0.9 ? 1 : 3;
  }

  /**
   * @param {{ x: number, y: number }} a
   * @param {{ x: number, y: number }} b
   * @returns {{ x: number, y: number }}
   */
  function add(a, b) {
    return { x: a.x + b.x, y: a.y + b.y };
  }

  /**
   * @param {{ x: number, y: number }} a
   * @param {{ x: number, y: number }} b
   * @returns {{ x: number, y: number }}
   */
  function sub(a, b) {
    return { x: a.x - b.x, y: a.y - b.y };
  }

  /**
   * @param {{ x: number, y: number }} point
   * @param {number} amount
   * @returns {{ x: number, y: number }}
   */
  function scale(point, amount) {
    return { x: point.x * amount, y: point.y * amount };
  }

  /**
   * @param {{ x: number, y: number }} point
   * @returns {{ x: number, y: number }}
   */
  function unit(point) {
    const length = Math.hypot(point.x, point.y);
    return { x: point.x / length, y: point.y / length };
  }

  /**
   * @param {number} value
   * @param {number} min
   * @param {number} max
   * @returns {number}
   */
  function clamp(value, min, max) {
    return Math.min(max, Math.max(min, value));
  }

  /**
   * @param {{ x: number, y: number }} tangent
   * @returns {{ x: number, y: number }}
   */
  function outwardNormal(tangent) {
    const normal = { x: -tangent.y, y: tangent.x };
    return normal.y >= 0 ? normal : scale(normal, -1);
  }

  /**
   * @param {{ x: number, y: number }} normal
   * @returns {number}
   */
  function rectRotation(normal) {
    return ((Math.atan2(normal.y, normal.x) * 180) / Math.PI) - 90;
  }

  /**
   * @param {{ x: number, y: number }} tangent
   * @returns {number}
   */
  function textRotation(tangent) {
    return (Math.atan2(tangent.y, tangent.x) * 180) / Math.PI;
  }

  /**
   * @param {string} text
   * @param {number} pathLength
   * @returns {number}
   */
  function textGapPos(text, pathLength) {
    return textGap(text) / pathLength;
  }

  /**
   * @param {string} text
   * @returns {number}
   */
  function textGap(text) {
    if (text === "FILTER") {
      return 18 + (text.length * 6);
    }

    return 12 + (text.replace(/\s+/g, "").length * 6);
  }

  /**
   * @param {number} start
   * @param {number} end
   * @param {number} amount
   * @returns {number}
   */
  function lerp(start, end, amount) {
    return start + ((end - start) * amount);
  }

  /**
   * @param {number} value
   * @returns {string}
   */
  function fmt(value) {
    return value.toFixed(2);
  }
})();
