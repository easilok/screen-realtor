# Screen Realtor - Screen output manage

## Introduction

*Screen Realtor* is a personal display output manager that, provided a configuration, can automatically select the preferred layout configuration of the outputs connected to a Linux system using X11.

It's written in common lisp, as part of my language learning journey.

The project born from a personal battle of automatically setting up may screen real estate in different layouts based on my location. What started as a set of bash scripts for calling *xrandr* with appropriate options (and even using current WiFi to guess my location), quickly fell as a new computer used dynamic output naming making my static configurations not working.

So, picking on my learning of the common lisp language, I decided to make a small tool that was smarter in selecting my preferred layout based on a set of conditions. Applied to a specific DSL for configuration, the possibilities for expanding its automatic selection are infinite.

This project is currently under heavy development. Refer to the [To-Do](./TODO.md) section to know what is being worked on at the moment.

## Running

### Development 

As this is a common lisp application, the following packages are required for it to work:

- [sbcl](https://www.sbcl.org) - the compiler;
- [quicklisp](https://www.quicklisp.org/beta/) - the common lisp library manager.

Having the proper setup, the application will install the required packages and run if used the following command:

```lisp
sbcl --load run.lisp
```

For easiness of development, a `nix development` shell is supplied with the proper development setup (quicklisp is still required to be installed manually).

### Production

The application can be run as a standalone binary. It first need to be built though which can be accomplished by running `make` in the root of the repository.
repository.

## Configuration

Configuration is done into a `config.disp` file that can be placed in your `~/.config/screen-realtor/`.

The configuration layout follows a lisp syntax and leverages two different calls: `define-layout` and `define-rule`. Refer to the provided [config.example.disp](./config.example.disp) example and make it yours.

### define-layout

Defines a screen layout with a least one output. Definition should have an unique name followed by a list of `outputs`. The following example defines a layout with two outputs, one assigned to the `eDP-1` interface by the system, and the other matching any other name.

- The `:mode` key defines the resolution and refresh rate; 
- The `:position` key defines the coordinates to place the output;
- The `:primary` key defines what output should be the primary.

```lisp
(define-layout external-top
  ((output (name= "eDP-1")
           :mode (1920 1200 120)
           :position (0 1080)
           :primary t)
   (output (not-name= "eDP-1")
           :mode (1920 1080)
           :position (0 0))))
```

### define-rule

Rules are conditions that trigger a specific layout. Currently only the connected WiFi is usable as a trigger, and can be set to an exact name or part of the name _(we are working on other conditions)_.

The application loops all `define-rules` and applies the first that has its `:predicate` function as true. The sequence the rules are placed when running the application is:

- Rule are sorted by their `:priority` from lowest to highest value;
- Rules with no priority set take a default of 100 plus their position (to ensure first declared rules should have a highest priority).

The following example defines a rule that sets the `external-top` layout when the computer is connected to the `WiFi-Example-1`, and sets its priority to 1 (**high priority**):

``` lisp
(define-rule top-layout-1
  :predicate (wifi= "WiFi-Example-1")
  :priority 1
  :layout external-top)
```

To define the same rule but triggered whenever the computer is connected to an WiFi SSID that includes `WiFi-Example` in the name:

``` lisp
(define-rule top-layout-1
  :predicate (wifi-includes "WiFi-Example")
  :priority 1
  :layout external-top)
```

The following rule defines a catch all that will always set the `laptop-only` layout as it has no predicate (and because of that it should always be the last rule):

``` lisp
(define-rule default-layout
  :layout laptop-only)
```

**Note:** The rule priority should not be set to a value higher that 99 to allow proper deprioritization of rules with no priority set.

### define-trigger

Define specific custom actions that are executed on triggers from layout changes. There are two possible triggers at the moment:

- `before-layout`, that can contain a list of actions to be called when the application is about to perform a layout update
- `after-layout`, that can contain a list of actions to be called after the application applies a new layout

The following example defines a trigger which actions will be called after a layout is set. This example has two actions that are run sequentially.

```lisp
(define-trigger after-layout
 ((run-shell :action "set-my-wallpaper")
  (run-shell :action "notify-send 'Layout Updated'")))
```
