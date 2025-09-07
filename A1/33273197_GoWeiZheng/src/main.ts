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
    mergeMap,
    startWith
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
    CX: (Viewport.CANVAS_WIDTH * 0.3), // centre x pos of birb
} as const;

const Constants = {
    PIPE_WIDTH: 50,
    PIPE_SPEED: 5,
    TICK_RATE_MS: 20, // Might need to change this!
    GRAVITY: 0.7,
    MAX_VY: 7,
    SEED: 1234,
    INVINCIBILITY_TIME: 3000,
} as const;

// User input

type Key = "Space" | "KeyR";

// State processing

type State = Readonly<{
    gameEnd: boolean,
    birdY: number,
    birdVy: number,
    pipes: ReadonlyArray<Pipe>,
    lives: number,
    invincibility: number,
    score: number,
}>;

const initialState: State = {
    gameEnd: false,
    birdY: Viewport.CANVAS_HEIGHT / 2 - Birb.HEIGHT / 2,
    birdVy: 0,
    pipes: [],
    lives: 3,
    invincibility: 0,
    score: 0,
};

type Pipe = Readonly<{
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
    return (s: State) => {
        // Add birb to the main grid canvas

        // Remove old birb element from previous frame
        if (document.querySelector(".player")) {
            document.querySelector(".player")!.remove();
        }

        // Create new birb element of updated Y position
        // Combining this with the remove logic, this creates
        // an effect of the birb moving
        const birdImg = createSvgElement(svg.namespaceURI, "image", {
            class: "player",
            href: "assets/birb.png",
            x: `${Viewport.CANVAS_WIDTH * 0.3 - Birb.WIDTH / 2}`,
            y: `${s.birdY}`,
            width: `${Birb.WIDTH}`,
            height: `${Birb.HEIGHT}`,
        });
        svg.appendChild(birdImg);
        
        // Value for giving birb a blinking effect
        const invincibleEffect = (Math.floor(s.invincibility / 100) % 2 ? "0.4" : "1");
        birdImg.setAttribute("y", `${s.birdY - Birb.HEIGHT / 2}`);
        // This is where the blinking effect happens
        birdImg.setAttribute("opacity", s.invincibility > 0 ? invincibleEffect : "1");

        // Same logic for birb but this time handling multiple pipes at the same time
        document.querySelectorAll(".pipes").forEach(pipe => pipe.remove());

        s.pipes.forEach(pipe => {
            const pipeTop = createSvgElement(svg.namespaceURI, "rect", {
                class: "pipes",
                x: `${String(pipe.x)}`,
                y: "0",
                width: `${Constants.PIPE_WIDTH}`,
                height: `${pipe.gapY - (pipe.gapH / 2)}`,
                fill: "green",
            })

            const pipeBottom = createSvgElement(svg.namespaceURI, "rect", {
                class: "pipes",
                x: `${String(pipe.x)}`,
                y: `${pipe.gapY + (pipe.gapH / 2)}`,
                width: `${Constants.PIPE_WIDTH}`,
                height: `${Viewport.CANVAS_HEIGHT - (pipe.gapY + (pipe.gapH / 2))}`,
                fill: "green",
            })

            svg.appendChild(pipeTop);
            svg.appendChild(pipeBottom);
        })

        livesText.textContent = `${s.lives}`;
        scoreText.textContent = `${s.score}`;

        if(s.gameEnd) {
            show(gameOver);
        }
        else{
            hide(gameOver);
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

    /**
     * A random number generator which provides two pure functions
     * `hash` and `scale`. Call `hash` repeatedly to generate the
     * sequence of hashes.
     * 
     * THIS CODE WAS TAKEN FROM APPLIED 4
     */
    abstract class RNG {
        private static m = 0x80000000; // 2^31
        private static a = 1103515245;
        private static c = 12345;

        public static hash = (seed: number): number =>
            (RNG.a * seed + RNG.c) % RNG.m;

        public static scale = (hash: number): number =>
            (2 * hash) / (RNG.m - 1) - 1; // in [-1, 1]
    }

    /**
     * THIS CODE WAS FROM APPLIED 4 EXERCISE 1
     */
    function createRngStreamFromSource<T>(source$: Observable<T>) {
        return function createRngStream(
            seed: number = 0,
        ): Observable<number> {
            const randomNumberStream = source$.pipe(
                scan((acc) => RNG.hash(acc), seed),
                map(RNG.scale)
            );

            return randomNumberStream;
        };
    }

    /** Determines the rate of time steps */
    const tick$ = createRngStreamFromSource(interval(Constants.TICK_RATE_MS))(Constants.SEED).pipe(
        map(rand => (s: State) => {
            // new state attributes to pass if no special event happens
            const vy = s.birdVy + Constants.GRAVITY;
            const clampedVy = Math.min(Constants.MAX_VY, vy);
            const y = s.birdY + vy;

            // simulate pipe moving to left
            const movePipes = s.pipes.map(p => ({
                ...p,
                x: p.x - Constants.PIPE_SPEED,
            }))

            // Determines which pipes to keep, pipes that moved to the left of the screen are removed
            const keepPipes: ReadonlyArray<Pipe> = movePipes.filter(p => (p.x + Constants.PIPE_WIDTH > 0));

            const bounceDown = 4 + 3 * rand
            const bounceUp = -4 + 3 * rand
            const newInv = Math.max(0, s.invincibility - Constants.TICK_RATE_MS); // Update invincibility duration
            const isInvincible = newInv > 0;

            // Helper function for determining how to update state when birb collides with an object
            const collided = (bounce: number) => {
                const newY = s.birdY + bounce;
                const newLives = isInvincible ? s.lives : Math.max(0, s.lives - 1);
                return{
                    ...s,
                    birdVy: bounce,
                    birdY: newY,
                    pipes: keepPipes,
                    lives: newLives,
                    invincibility: isInvincible ? newInv : Constants.INVINCIBILITY_TIME,
                    gameEnd: newLives === 0,
                }   
            };

            // determines the collision box of the birb
            const birbTop = s.birdY - (Birb.HEIGHT / 2);
            const birbBottom = s.birdY + (Birb.HEIGHT / 2);
            const birbLeft = Birb.CX - (Birb.WIDTH / 2);
            const birbRight = Birb.CX + (Birb.WIDTH / 2);

            // collision logic for birb with pipe
            const handleCollisions = keepPipes.some(
                p => {
                    const pipeLeft = p.x;
                    const pipeRight = p.x + Constants.PIPE_WIDTH;
                    const gapTop = p.gapY - (p.gapH / 2);
                    const gapBottom = p.gapY + (p.gapH / 2);

                    const horizontal = (birbRight > pipeLeft) && (birbLeft < pipeRight);
                    const vertical = (birbTop < gapTop) || (birbBottom > gapBottom);
                    return horizontal && vertical;
                }
            )

            if (s.birdY >= (Viewport.CANVAS_HEIGHT - Birb.HEIGHT / 2)) return collided(bounceUp); // hit bottom edge
            if (s.birdY <= (0 + Birb.HEIGHT / 2)) return collided(bounceDown); // hit top edge
            if (handleCollisions) return (s.birdVy > 0 ? collided(bounceUp) : collided(bounceDown)) // collide with pipe

            // Determines whether the birb has passed a pipe or not
            const passPipe = keepPipes.some(
                p => {
                    const prevRight = p.x + Constants.PIPE_WIDTH;
                    const nextRight = (p.x - Constants.PIPE_SPEED) + Constants.PIPE_WIDTH;
                    return (Birb.CX <= prevRight && Birb.CX > nextRight);
                }
            )

            // Score increment when passing one pipe
            if (passPipe) return {
                ...s,
                birdVy: clampedVy,
                birdY: y,
                pipes: keepPipes,
                invincibility: newInv,
                score: s.score + 1,
            }

            // End the game when birb has passed through all the pipes
            const numOfPipes = csvContents.split(/\r?\n/).slice(1).length;
            if (s.score === numOfPipes) {
                return {
                    ...s,
                    gameEnd: true,
                }
            }

            // update state normally when no special event
            return {
                ...s,
                birdVy: clampedVy,
                birdY: y,
                pipes: keepPipes,
                invincibility: newInv,
            }
        })
    );

    // function for parsing csv, outputs data for spawning pipes
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

    // pipes' data
    const pipes = parseMap(csvContents);

    // stream that update the state's pipes
    const pipes$ = from(pipes).pipe(
        mergeMap(({gapY, gapH, time}) => 
            timer(time).pipe(
                map(_ => (s: State): State => {
                    const p: Pipe = {
                        x: Viewport.CANVAS_WIDTH,
                        gapY: gapY,
                        gapH: gapH,
                    }

                    return {
                        ...s,
                        pipes: s.pipes.concat(p),
                    }
                })
            )
        )
    )

    // merge all substream into one stream then update the states
    const game$ = merge(flap$, tick$, pipes$).pipe(
        scan((state, reducerFn) => state.gameEnd ? state : reducerFn(state), initialState)
    )    

    // restarts the game
    const restart$ = fromKey("KeyR").pipe(
        startWith(null), // emit one value, this allows first game to run without R key inputs
        switchMap(_ => game$)
    );

    return restart$;
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
