CREATE TABLE "player" (
  "id" serial PRIMARY KEY,
  "nickname" text NOT NULL,
  "password" text NOT NULL,
  "token" text,
  "token_expiration" timestamp without time zone,
  "played_battles" int NOT NULL DEFAULT 0,
  "battles_won" int NOT NULL DEFAULT 0,
  "winrate" float NOT NULL DEFAULT 0,

  CONSTRAINT player_nickname_key UNIQUE (nickname),
  CONSTRAINT player_token_key UNIQUE (token)
);

CREATE TYPE event_type AS ENUM('registration',
                               'battle_start',
                               'battle_end',
                               'money_buying',
                               'achievement',
                               'login',
                               'logout');

CREATE TABLE "event" (
  "id" serial PRIMARY KEY,
  "player_id" int NOT NULL,
  "type" event_type NOT NULL,
  "source" int,
  "payload" jsonb,
  "created" timestamp without time zone DEFAULT timezone('UTC'::text, now())
);

CREATE TABLE "transaction" (
  "id" serial PRIMARY KEY,
  "event_id" int NOT NULL,
  "player_id" int NOT NULL,
  "currency_id" int NOT NULL,
  "amount" int
);

CREATE TYPE currency_type AS ENUM('silver',
                                  'gold');

CREATE TABLE "currency" (
  "id" serial PRIMARY KEY,
  "type" currency_type NOT NULL,
  "rate" float NOT NULL
);

CREATE TYPE achievement_type AS ENUM('5_battles',
                                     '5_wins',
                                     'first_win');

CREATE TABLE "achievement" (
  "id" serial PRIMARY KEY,
  "type" achievement_type NOT NULL,
  "player_id" int NOT NULL,
  "created" timestamp without time zone DEFAULT timezone('UTC'::text, now())
);

CREATE TABLE "money" (
  "id" serial PRIMARY KEY,
  "player_id" int NOT NULL,
  "currency_id" int NOT NULL,
  "amount" int
);

CREATE TABLE "battle" (
  "id" serial PRIMARY KEY,
  "currency_id" int NOT NULL,
  "bid" int,
  "winner" int NOT NULL,
  "participants" int[] NOT NULL,
  "created" timestamp without time zone DEFAULT timezone('UTC'::text, now())
);

ALTER TABLE "event" ADD FOREIGN KEY ("player_id") REFERENCES "player" ("id");

ALTER TABLE "transaction" ADD FOREIGN KEY ("event") REFERENCES "event" ("id");

ALTER TABLE "transaction" ADD FOREIGN KEY ("player_id") REFERENCES "player" ("id");

ALTER TABLE "transaction" ADD FOREIGN KEY ("currency_id") REFERENCES "currency" ("id");

ALTER TABLE "achievement" ADD FOREIGN KEY ("player_id") REFERENCES "player" ("id");

ALTER TABLE "money" ADD FOREIGN KEY ("player_id") REFERENCES "player" ("id");

ALTER TABLE "money" ADD FOREIGN KEY ("currency_id") REFERENCES "currency" ("id");

ALTER TABLE "battle" ADD FOREIGN KEY ("currency_id") REFERENCES "currency" ("id");

ALTER TABLE "battle" ADD FOREIGN KEY ("winner") REFERENCES "player" ("id");
