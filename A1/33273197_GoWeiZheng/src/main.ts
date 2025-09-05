/**
 * Inside this file you will use the classes and functions from rx.js
 * to add visuals to the svg element in index.html, animate them, and make them interactive.
 *
 * Study and complete the tasks in observable exercises first to get ideas.
 *
 * Course Notes showing Asteroids in FRP: https://tgdwyer.github.io/asteroids/
 *
 * You will be marked on your functional programming style
 * as well as the functionality that you implement.
 *
 * Document your code!
 */

import "./style.css";

import {
    Observable,
    catchError,
    filter,
    fromEvent,
    interval,
    map,
    scan,
    switchMap,
    take,
    merge,
    timer,
    from,
    mergeMap
} from "rxjs";
import { fromFetch } from "rxjs/fetch";

/** Constants */

const Viewport = {
    CANVAS_WIDTH: 600,
    CANVAS_HEIGHT: 400,
} as const;

const Birb = {
    WIDTH: 42,
    HEIGHT: 30,
} as const;

const Constants = {
    PIPE_WIDTH: 50,
    PIPE_SPEED: 2,
    TICK_RATE_MS: 20, // Might need to change this!
    GRAVITY: 0.7,
    MAX_VY: 7,
} as const;

// User input

type Key = "Space";

// State processing

type State = Readonly<{
    gameEnd: boolean,
    birdY: number,
    birdVy: number,
    pipes: ReadonlyArray<Pipe>,
    exit: ReadonlyArray<string>,
    nextPipeId: number,
    lives: number,
}>;

const initialState: State = {
    gameEnd: false,
    birdY: Viewport.CANVAS_HEIGHT / 2 - Birb.HEIGHT / 2,
    birdVy: 0,
    pipes: [],
    exit: [],
    nextPipeId: 0,
    lives: 3,
};

type Pipe = Readonly<{
    id: string,
    x: number,
    gapY: number,
    gapH: number,
}>;

/**
 * Updates the state by proceeding with one time step.
 *
 * @param s Current state
 * @returns Updated state
 */
const tick = (s: State) => s;

// Rendering (side effects)

/**
 * Brings an SVG element to the foreground.
 * @param elem SVG element to bring to the foreground
 */
const bringToForeground = (elem: SVGElement): void => {
    elem.parentNode?.appendChild(elem);
};

/**
 * Displays a SVG element on the canvas. Brings to foreground.
 * @param elem SVG element to display
 */
const show = (elem: SVGElement): void => {
    elem.setAttribute("visibility", "visible");
    bringToForeground(elem);
};

/**
 * Hides a SVG element on the canvas.
 * @param elem SVG element to hide
 */
const hide = (elem: SVGElement): void => {
    elem.setAttribute("visibility", "hidden");
};

/**
 * Creates an SVG element with the given properties.
 *
 * See https://developer.mozilla.org/en-US/docs/Web/SVG/Element for valid
 * element names and properties.
 *
 * @param namespace Namespace of the SVG element
 * @param name SVGElement name
 * @param props Properties to set on the SVG element
 * @returns SVG element
 */
const createSvgElement = (
    namespace: string | null,
    name: string,
    props: Record<string, string> = {},
): SVGElement => {
    const elem = document.createElementNS(namespace, name) as SVGElement;
    Object.entries(props).forEach(([k, v]) => elem.setAttribute(k, v));
    return elem;
};

const render = (): ((s: State) => void) => {
    // Canvas elements
    const gameOver = document.querySelector("#gameOver") as SVGElement;
    const container = document.querySelector("#main") as HTMLElement;

    // Text fields
    const livesText = document.querySelector("#livesText") as HTMLElement;
    const scoreText = document.querySelector("#scoreText") as HTMLElement;

    const svg = document.querySelector("#svgCanvas") as SVGSVGElement;

    svg.setAttribute(
        "viewBox",
        `0 0 ${Viewport.CANVAS_WIDTH} ${Viewport.CANVAS_HEIGHT}`,
    );
    /**
     * Renders the current state to the canvas.
     *
     * In MVC terms, this updates the View using the Model.
     *
     * @param s Current state
     */
    
    // Add birb to the main grid canvas
    const birdImg = createSvgElement(svg.namespaceURI, "image", {
        href: "assets/birb.png",
        x: `${Viewport.CANVAS_WIDTH * 0.3 - Birb.WIDTH / 2}`,
        y: `${Viewport.CANVAS_HEIGHT / 2 - Birb.HEIGHT / 2}`,
        width: `${Birb.WIDTH}`,
        height: `${Birb.HEIGHT}`,
    });
    svg.appendChild(birdImg);
    
    const createPipe = (id: string) => {
        const g = createSvgElement(svg.namespaceURI, "g", { id }) as SVGGElement;
        const top = createSvgElement(svg.namespaceURI, "rect", {
                                                                "data-part": "top",
                                                                x: `${Viewport.CANVAS_WIDTH}`,
                                                                y: "0",
                                                                width: `${Constants.PIPE_WIDTH}`,
                                                                fill: "green"
                                                            });
        const bottom = createSvgElement(svg.namespaceURI, "rect", {
                                                                    "data-part": "bottom",
                                                                    x: `${Viewport.CANVAS_WIDTH}`,
                                                                    width: `${Constants.PIPE_WIDTH}`,
                                                                    fill: "green"
                                                                });
        g.appendChild(top);
        g.appendChild(bottom);
        svg.appendChild(g);
        return g;
    }

    // // Draw a static pipe as a demonstration
    // const pipeGapY = 200; // vertical center of the gap
    // const pipeGapHeight = 100;

    // // Top pipe
    // const pipeTop = createSvgElement(svg.namespaceURI, "rect", {
    //     x: "150",
    //     y: "0",
    //     width: `${Constants.PIPE_WIDTH}`,
    //     height: `${pipeGapY - pipeGapHeight / 2}`,
    //     fill: "green",
    // });

    // // Bottom pipe
    // const pipeBottom = createSvgElement(svg.namespaceURI, "rect", {
    //     x: "150",
    //     y: `${pipeGapY + pipeGapHeight / 2}`,
    //     width: `${Constants.PIPE_WIDTH}`,
    //     height: `${Viewport.CANVAS_HEIGHT - (pipeGapY + pipeGapHeight / 2)}`,
    //     fill: "green",
    // });

    // svg.appendChild(pipeTop);
    // svg.appendChild(pipeBottom);
    return (s: State) => {
        birdImg.setAttribute("y", `${s.birdY - Birb.HEIGHT / 2}`)

        for (const p of s.pipes) {
            const g = (document.getElementById(p.id) as SVGGElement) ?? createPipe(p.id);
            const top = g.querySelector('rect[data-part="top"]');
            const bottom = g.querySelector('rect[data-part="bottom"]');

            top?.setAttribute("x", String(p.x));
            top?.setAttribute("height", `${p.gapY - p.gapH / 2}`);

            bottom?.setAttribute("x", String(p.x));
            bottom?.setAttribute("y", `${p.gapY + p.gapH / 2}`);
            bottom?.setAttribute("height", `${Viewport.CANVAS_HEIGHT - (p.gapY + p.gapH / 2)}`);
        }
    }   
};

export const state$ = (csvContents: string): Observable<State> => {
    /** User input */

    const key$ = fromEvent<KeyboardEvent>(document, "keypress");
    const fromKey = (keyCode: Key) =>
        key$.pipe(filter(({ code }) => code === keyCode));

    const flap$ = fromKey('Space').pipe(
        map(_ => (s: State) => {
            return {
                ...s,
                birdVy: -7,
            }
        })
    )

    /** Determines the rate of time steps */
    const tick$ = interval(Constants.TICK_RATE_MS).pipe(
        map(_ => (s: State) => {
            const vy = s.birdVy + Constants.GRAVITY;
            const clampedVy = Math.min(Constants.MAX_VY, vy);
            const y = s.birdY + vy;

            const movePipes = s.pipes.map(p => ({
                ...p,
                x: p.x - Constants.PIPE_SPEED,
            }))
            const removePipes: ReadonlyArray<string> = movePipes.filter(p => (p.x + Constants.PIPE_WIDTH < 0)).map(p => p.id);;
            const keepPipes: ReadonlyArray<Pipe> = movePipes.filter(p => (p.x + Constants.PIPE_WIDTH > 0))

            return {
                ...s,
                birdVy: clampedVy,
                birdY: y,
                pipes: keepPipes,
                exit: removePipes,
            }
        })
    );

    const parseMap = (csv: string) => {
        return csv
        .split(/\r?\n/)
        .map(line => line.split(","))
        .slice(1)   // remove header
        .map(([gapY, gapH, time]) => ({
            gapY: Number(gapY) * Viewport.CANVAS_HEIGHT,
            gapH: Number(gapH) * Viewport.CANVAS_HEIGHT,
            time: Number(time) * 1000,
        }))
    }

    const pipes = parseMap(csvContents);

    const pipes$ = from(pipes).pipe(
        mergeMap(({gapY, gapH, time}) => 
            timer(time).pipe(
                map(_ => (s: State): State => {
                    const id = `pipe-${s.nextPipeId}`
                    const p: Pipe = {
                        id: id,
                        x: Viewport.CANVAS_WIDTH,
                        gapY: gapY,
                        gapH: gapH,
                    }

                    return {
                        ...s,
                        nextPipeId: s.nextPipeId + 1,
                        pipes: s.pipes.concat(p),
                    }
                })
            )
        )
    )

    return merge(flap$, tick$, pipes$).pipe(
        scan((state, reducerFn) => reducerFn(state), initialState)
    )
};

// The following simply runs your main function on window load.  Make sure to leave it in place.
// You should not need to change this, beware if you are.
if (typeof window !== "undefined") {
    const { protocol, hostname, port } = new URL(import.meta.url);
    const baseUrl = `${protocol}//${hostname}${port ? `:${port}` : ""}`;
    const csvUrl = `${baseUrl}/assets/map.csv`;

    // Get the file from URL
    const csv$ = fromFetch(csvUrl).pipe(
        switchMap(response => {
            if (response.ok) {
                return response.text();
            } else {
                throw new Error(`Fetch error: ${response.status}`);
            }
        }),
        catchError(err => {
            console.error("Error fetching the CSV file:", err);
            throw err;
        }),
    );

    // Observable: wait for first user click
    const click$ = fromEvent(document.body, "mousedown").pipe(take(1));

    csv$.pipe(
        switchMap(contents =>
            // On click - start the game
            click$.pipe(switchMap(() => state$(contents))),
        ),
    ).subscribe(render());
}
