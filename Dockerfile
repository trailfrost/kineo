# ---- Base Image ----
FROM node:22-slim

# ---- System Setup ----
ENV PNPM_HOME="/usr/local/share/pnpm"
ENV PATH="$PNPM_HOME:$PATH"
ENV NODE_ENV=development

# Install pnpm globally and minimal tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    bash curl ca-certificates \
    && corepack enable \
    && corepack prepare pnpm@latest --activate \
    && rm -rf /var/lib/apt/lists/*

# ---- Create App Directory ----
WORKDIR /app

# ---- Dependency Installation ----
# Copy only the package files first to leverage Docker caching
COPY package.json pnpm-lock.yaml* ./

# Install dependencies with pnpm
RUN pnpm install --frozen-lockfile

# ---- Copy Source ----
COPY . .

# ---- Default Command ----
CMD ["pnpm", "test"]
