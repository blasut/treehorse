--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner:
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner:
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: fields; Type: TABLE; Schema: public; Owner: treehorse; Tablespace:
--

CREATE TABLE fields (
    id integer NOT NULL,
    type character varying(40) NOT NULL,
    name character varying(50) NOT NULL,
    "values" character varying(200),
    "timestamp" timestamp without time zone DEFAULT now()
);


ALTER TABLE fields OWNER TO treehorse;

--
-- Name: firstkey; Type: CONSTRAINT; Schema: public; Owner: treehorse; Tablespace:
--

ALTER TABLE ONLY fields
    ADD CONSTRAINT firstkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--
