# Stage 1: build the binary
FROM debian:bookworm-slim AS builder

RUN apt-get update && \
    apt-get install -y sbcl curl openssl ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

COPY . .

# Download Quicklisp installer
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp

# Build the binary
RUN sbcl --no-userinit --no-sysinit --non-interactive \
         --load release.lisp

# Stage 2: build the binary
FROM scratch AS binary

COPY --from=builder /build/lp-displayer /
