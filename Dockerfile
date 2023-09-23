FROM alpine:3.18 AS builder

RUN apk update && \
    apk add \
        curl \
        xz

ARG ZIGVER=0.12.0-dev.298+acc947191
ARG ARCH=x86_64

RUN mkdir -p /build/deps
WORKDIR /build
RUN curl https://ziglang.org/builds/zig-linux-$ARCH-$ZIGVER.tar.xz  -O && \
    tar xf zig-linux-$ARCH-$ZIGVER.tar.xz && \
    mv zig-linux-$ARCH-$ZIGVER deps/

COPY build.zig .
COPY src ./src
RUN ./deps/zig-linux-$ARCH-$ZIGVER/zig build

FROM alpine:3.18
COPY --from=builder /build/zig-out/ .

ENTRYPOINT ["./bin/rinha_zig"]