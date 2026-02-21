# cl-deps — Canonical Common Lisp dependency set
#
# Consumed as a flake input by kli and other CL projects.

{ pkgs, lib }:

let
  buildLisp = import ./buildLisp.nix { inherit pkgs lib; };

  lisp = rec {

    # ===================================================================
    # SBCL bundled modules
    # ===================================================================

    uiop = buildLisp.bundled "uiop";
    asdf = buildLisp.bundled "asdf";
    sb-bsd-sockets = buildLisp.bundled "sb-bsd-sockets";
    sb-cltl2 = buildLisp.bundled "sb-cltl2";
    sb-posix = buildLisp.bundled "sb-posix";
    sb-rotate-byte = buildLisp.bundled "sb-rotate-byte";

    # ===================================================================
    # Tier 0: leaf libraries (no CL deps)
    # ===================================================================

    alexandria = let src = pkgs.srcOnly pkgs.sbcl.pkgs.alexandria; in
      buildLisp.library {
        name = "alexandria";
        srcs = map (f: src + ("/alexandria-1/" + f)) [
          "package.lisp"
          "definitions.lisp"
          "binding.lisp"
          "strings.lisp"
          "conditions.lisp"
          "symbols.lisp"
          "macros.lisp"
          "functions.lisp"
          "io.lisp"
          "hash-tables.lisp"
          "control-flow.lisp"
          "lists.lisp"
          "types.lisp"
          "arrays.lisp"
          "sequences.lisp"
          "numbers.lisp"
          "features.lisp"
        ];
      };

    trivial-features = let src = pkgs.srcOnly pkgs.sbcl.pkgs.trivial-features; in
      buildLisp.library {
        name = "trivial-features";
        srcs = [ (src + "/src/tf-sbcl.lisp") ];
      };

    trivial-garbage = let src = pkgs.srcOnly pkgs.sbcl.pkgs.trivial-garbage; in
      buildLisp.library {
        name = "trivial-garbage";
        srcs = [ (src + "/trivial-garbage.lisp") ];
      };

    global-vars = let src = pkgs.srcOnly pkgs.sbcl.pkgs.global-vars; in
      buildLisp.library {
        name = "global-vars";
        srcs = [ "${src}/global-vars.lisp" ];
      };

    trivial-gray-streams = let src = pkgs.srcOnly pkgs.sbcl.pkgs.trivial-gray-streams; in
      buildLisp.library {
        name = "trivial-gray-streams";
        srcs = [
          (src + "/package.lisp")
          (src + "/streams.lisp")
        ];
      };

    cl-ppcre = let src = pkgs.srcOnly pkgs.sbcl.pkgs.cl-ppcre; in
      buildLisp.library {
        name = "cl-ppcre";
        srcs = map (f: src + ("/" + f)) [
          "packages.lisp"
          "specials.lisp"
          "util.lisp"
          "errors.lisp"
          "charset.lisp"
          "charmap.lisp"
          "chartest.lisp"
          "lexer.lisp"
          "parser.lisp"
          "regex-class.lisp"
          "regex-class-util.lisp"
          "convert.lisp"
          "optimize.lisp"
          "closures.lisp"
          "repetition-closures.lisp"
          "scanner.lisp"
          "api.lisp"
        ];
      };

    cl-base64 = let src = pkgs.srcOnly pkgs.sbcl.pkgs.cl-base64; in
      buildLisp.library {
        name = "cl-base64";
        srcs = [
          (src + "/package.lisp")
          (src + "/encode.lisp")
          (src + "/decode.lisp")
        ];
      };

    trivial-mimes = let src = pkgs.fetchFromGitHub {
      owner = "Shinmera"; repo = "trivial-mimes";
      rev = "master";
      sha256 = "sha256-Ics5tsAIWXqtIyhoa68JpKCaq4AklEk9fI/RjuDMKnE=";
    }; in
      buildLisp.library {
        name = "trivial-mimes";
        srcs = [ "${src}/mime-types.lisp" ];
      };

    cl-utilities = let src = pkgs.srcOnly pkgs.sbcl.pkgs.cl-utilities; in
      buildLisp.library {
        name = "cl-utilities";
        srcs = map (f: src + ("/" + f)) [
          "package.lisp"
          "with-unique-names.lisp"
          "once-only.lisp"
          "compose.lisp"
          "split-sequence.lisp"
          "extremum.lisp"
          "read-delimited.lisp"
          "expt-mod.lisp"
          "collecting.lisp"
          "rotate-byte.lisp"
          "copy-array.lisp"
        ];
      };

    split-sequence = let src = pkgs.srcOnly pkgs.sbcl.pkgs.split-sequence; in
      buildLisp.library {
        name = "split-sequence";
        srcs = map (f: src + ("/" + f)) [
          "package.lisp"
          "vector.lisp"
          "list.lisp"
          "extended-sequence.lisp"
          "api.lisp"
          "documentation.lisp"
        ];
      };

    rfc2388 = let src = pkgs.srcOnly pkgs.sbcl.pkgs.rfc2388; in
      buildLisp.library {
        name = "rfc2388";
        srcs = map (f: src + ("/" + f)) [
          "packages.lisp"
          "rfc2388.lisp"
        ];
      };

    trivial-backtrace = let src = pkgs.srcOnly pkgs.sbcl.pkgs.trivial-backtrace; in
      buildLisp.library {
        name = "trivial-backtrace";
        srcs = map (f: src + ("/dev/" + f)) [
          "packages.lisp"
          "utilities.lisp"
          "backtrace.lisp"
          "map-backtrace.lisp"
          "fallback.lisp"
        ];
      };

    named-readtables = let src = pkgs.srcOnly pkgs.sbcl.pkgs.named-readtables; in
      buildLisp.library {
        name = "named-readtables";
        srcs = map (f: src + ("/src/" + f)) [
          "package.lisp"
          "utils.lisp"
          "define-api.lisp"
          "cruft.lisp"
          "named-readtables.lisp"
        ];
      };

    closer-mop = let src = pkgs.srcOnly pkgs.sbcl.pkgs.closer-mop; in
      buildLisp.library {
        name = "closer-mop";
        srcs = [
          "${src}/closer-mop-packages.lisp"
          "${src}/closer-mop-shared.lisp"
          "${src}/closer-sbcl.lisp"
        ];
      };

    trivial-cltl2 = let src = pkgs.srcOnly pkgs.sbcl.pkgs.trivial-cltl2; in
      buildLisp.library {
        name = "trivial-cltl2";
        srcs = [ (src + "/cltl2.lisp") ];
      };

    chipz = let src = pkgs.srcOnly pkgs.sbcl.pkgs.chipz; in
      buildLisp.library {
        name = "chipz";
        deps = [ asdf ];
        srcs = map (f: src + ("/" + f)) [
          "chipz.asd"
          "package.lisp"
          "constants.lisp"
          "conditions.lisp"
          "dstate.lisp"
          "types-and-tables.lisp"
          "crc32.lisp"
          "adler32.lisp"
          "inflate-state.lisp"
          "gzip.lisp"
          "zlib.lisp"
          "inflate.lisp"
          "bzip2.lisp"
          "decompress.lisp"
          "stream.lisp"
        ];
      };

    md5 = let src = pkgs.srcOnly pkgs.sbcl.pkgs.md5; in
      buildLisp.library {
        name = "md5";
        deps = [ sb-rotate-byte ];
        srcs = [ (src + "/md5.lisp") ];
      };

    # --- New from core ---

    iterate = let src = pkgs.srcOnly pkgs.sbcl.pkgs.iterate; in
      buildLisp.library {
        name = "iterate";
        srcs = [
          "${src}/package.lisp"
          "${src}/iterate.lisp"
        ];
      };

    cl-who = let src = pkgs.srcOnly pkgs.sbcl.pkgs.cl-who; in
      buildLisp.library {
        name = "cl-who";
        srcs = map (f: src + ("/" + f)) [
          "packages.lisp"
          "specials.lisp"
          "util.lisp"
          "who.lisp"
        ];
      };

    anaphora = let src = pkgs.srcOnly pkgs.sbcl.pkgs.anaphora; in
      buildLisp.library {
        name = "anaphora";
        srcs = map (f: src + ("/" + f)) [
          "packages.lisp"
          "early.lisp"
          "symbolic.lisp"
          "anaphora.lisp"
        ];
      };

    event-emitter = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "event-emitter";
      rev = "master";
      sha256 = "sha256-1l+bI8xEfK5hDZbCyvyw454BXZpvb2GswVEvnkA+R04=";
    }; in
      buildLisp.library {
        name = "event-emitter";
        srcs = [ "${src}/src/event-emitter.lisp" ];
      };

    sha1 = let src = pkgs.fetchFromGitHub {
      owner = "massung"; repo = "sha1";
      rev = "master";
      sha256 = "sha256-n3RMSHmzQ2UQSnPR/uzQvDb0Ycp8nNpE5hNz54sE1rE=";
    }; in
      buildLisp.library {
        name = "sha1";
        srcs = [ "${src}/sha1.lisp" ];
      };

    # ===================================================================
    # Tier 1: one level of deps
    # ===================================================================

    bordeaux-threads = let
      src = pkgs.srcOnly pkgs.sbcl.pkgs.bordeaux-threads;
      getSrc = f: "${src}/${f}";
    in
      buildLisp.library {
        name = "bordeaux-threads";
        deps = [ alexandria global-vars trivial-features trivial-garbage ];
        srcs = map getSrc [
          "apiv1/pkgdcl.lisp"
          "apiv1/bordeaux-threads.lisp"
          "apiv1/impl-sbcl.lisp"
          "apiv1/default-implementations.lisp"
          "apiv2/pkgdcl.lisp"
          "apiv2/bordeaux-threads.lisp"
          "apiv2/timeout-interrupt.lisp"
          "apiv2/impl-sbcl.lisp"
          "apiv2/api-locks.lisp"
          "apiv2/api-threads.lisp"
          "apiv2/api-semaphores.lisp"
          "apiv2/api-condition-variables.lisp"
        ];
      };

    babel = let src = pkgs.srcOnly pkgs.sbcl.pkgs.babel; in
      buildLisp.library {
        name = "babel";
        deps = [ alexandria trivial-features ];
        srcs = map (f: src + ("/src/" + f)) [
          "packages.lisp"
          "encodings.lisp"
          "enc-ascii.lisp"
          "enc-ebcdic.lisp"
          "enc-ebcdic-int.lisp"
          "enc-iso-8859.lisp"
          "enc-unicode.lisp"
          "enc-cp1251.lisp"
          "enc-cp1252.lisp"
          "jpn-table.lisp"
          "enc-jpn.lisp"
          "enc-gbk.lisp"
          "enc-koi8.lisp"
          "external-format.lisp"
          "strings.lisp"
          "gbk-map.lisp"
          "sharp-backslash.lisp"
        ];
      };

    chunga = let src = pkgs.srcOnly pkgs.sbcl.pkgs.chunga; in
      buildLisp.library {
        name = "chunga";
        deps = [ trivial-gray-streams ];
        srcs = map (f: src + ("/" + f)) [
          "packages.lisp"
          "specials.lisp"
          "util.lisp"
          "known-words.lisp"
          "conditions.lisp"
          "read.lisp"
          "streams.lisp"
          "input.lisp"
          "output.lisp"
        ];
      };

    flexi-streams = let src = pkgs.srcOnly pkgs.sbcl.pkgs.flexi-streams; in
      buildLisp.library {
        name = "flexi-streams";
        deps = [ trivial-gray-streams ];
        srcs = map (f: src + ("/" + f)) [
          "packages.lisp"
          "mapping.lisp"
          "ascii.lisp"
          "koi8-r.lisp"
          "mac.lisp"
          "iso-8859.lisp"
          "enc-cn-tbl.lisp"
          "code-pages.lisp"
          "specials.lisp"
          "util.lisp"
          "conditions.lisp"
          "external-format.lisp"
          "length.lisp"
          "encode.lisp"
          "decode.lisp"
          "in-memory.lisp"
          "stream.lisp"
          "output.lisp"
          "input.lisp"
          "io.lisp"
          "strings.lisp"
        ];
      };

    idna = let src = pkgs.srcOnly pkgs.sbcl.pkgs.idna; in
      buildLisp.library {
        name = "idna";
        deps = [ split-sequence ];
        srcs = map (f: src + ("/" + f)) [
          "package.lisp"
          "encode.lisp"
          "decode.lisp"
        ];
      };

    lisp-namespace = let src = pkgs.srcOnly pkgs.sbcl.pkgs.lisp-namespace; in
      buildLisp.library {
        name = "lisp-namespace";
        deps = [ alexandria ];
        srcs = [
          (src + "/src/package.lisp")
          (src + "/src/namespace.lisp")
          (src + "/src/namespace-let.lisp")
        ];
      };

    fare-utils = let src = pkgs.srcOnly pkgs.sbcl.pkgs.fare-utils; in
      buildLisp.library {
        name = "fare-utils";
        deps = [ alexandria uiop ];
        srcs = [
          (src + "/package.lisp")
          (src + "/base/utils.lisp")
          (src + "/base/character-classes.lisp")
          (src + "/base/strings.lisp")
          (src + "/base/symbols.lisp")
          (src + "/base/macros.lisp")
          (src + "/base/lists.lisp")
          (src + "/base/packages.lisp")
          (src + "/base/objects.lisp")
          (src + "/base/streams.lisp")
          (src + "/base/hash-tables.lisp")
          (src + "/base/more-strings.lisp")
          (src + "/base/parse-cl-syntax.lisp")
          (src + "/filesystem/pathnames.lisp")
          (src + "/filesystem/files.lisp")
          (src + "/filesystem/atomic.lisp")
          (src + "/stateful/package.lisp")
          (src + "/stateful/container.lisp")
          (src + "/stateful/dllist.lisp")
        ];
      };

    # --- New from core ---

    nibbles = let src = pkgs.srcOnly (pkgs.sbcl.pkgs.nibbles.overrideAttrs (oldAttrs: {
      patches = oldAttrs.patches or [] ++ [
        (pkgs.fetchpatch {
          name = "nibbles-sbcl-x86-restore-weird-progn.patch";
          url = "https://github.com/sharplispers/nibbles/commit/f37322b864ea12018bc0acbd70cb1e24bf0426eb.patch";
          revert = true;
          sha256 = "0h601g145qscmvykrzrf9bnlakfh5qawwmdd1z8f2cslfxrkj9jc";
        })
      ];
    })); in
      buildLisp.library {
        name = "nibbles";
        deps = [ asdf ];
        srcs = map (f: src + ("/" + f)) [
          "package.lisp"
          "types.lisp"
          "macro-utils.lisp"
          "vectors.lisp"
          "streams.lisp"
          # SBCL-specific optimizations (cl-deps is SBCL-only)
          "sbcl-opt/fndb.lisp"
          "sbcl-opt/nib-tran.lisp"
          "sbcl-opt/x86-vm.lisp"
          "sbcl-opt/x86-64-vm.lisp"
        ];
      };

    # ===================================================================
    # Tier 2: two levels deep
    # ===================================================================

    cffi = let
      baseSrc = pkgs.srcOnly pkgs.sbcl.pkgs.cffi;
      src = pkgs.applyPatches {
        name = "cffi-source-patched";
        src = baseSrc;
        patches = [ ./cffi/cffi-initial-thread-timeout.patch ];
      };
    in
      buildLisp.library {
        name = "cffi";
        deps = [ alexandria babel trivial-features asdf ];
        srcs = [
          "${src}/src/package.lisp"
          "${src}/src/sys-utils.lisp"
          "${src}/src/cffi-sbcl.lisp"
        ] ++ map (f: src + ("/src/" + f)) [
          "utils.lisp"
          "libraries.lisp"
          "early-types.lisp"
          "types.lisp"
          "enum.lisp"
          "strings.lisp"
          "structures.lisp"
          "functions.lisp"
          "foreign-vars.lisp"
          "features.lisp"
        ];
      };

    proc-parse = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "proc-parse";
      rev = "master";
      sha256 = "sha256-gpNY32YrKMp86FhWRZHSTeckmPJYV1UZ5Z5gt4yQax8=";
    }; in
      buildLisp.library {
        name = "proc-parse";
        deps = [ alexandria babel sb-cltl2 ];
        srcs = [ "${src}/src/proc-parse.lisp" ];
      };

    xsubseq = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "xsubseq";
      rev = "master";
      sha256 = "sha256-/aaUy8um0lZpJXuBMrLO3azbfjSOR4n1cJRVcQFO5/c=";
    }; in
      buildLisp.library {
        name = "xsubseq";
        deps = [ sb-cltl2 ];
        srcs = [ "${src}/src/xsubseq.lisp" ];
      };

    cl-fad = let src = pkgs.srcOnly pkgs.sbcl.pkgs.cl-fad; in
      buildLisp.library {
        name = "cl-fad";
        deps = [ alexandria bordeaux-threads sb-posix ];
        srcs = map (f: src + ("/" + f)) [
          "packages.lisp"
          "fad.lisp"
          "path.lisp"
          "temporary-files.lisp"
        ];
      };

    trivia = let src = pkgs.srcOnly pkgs.sbcl.pkgs.trivia; in
      buildLisp.library {
        name = "trivia";
        deps = [ alexandria closer-mop lisp-namespace trivial-cltl2 ];
        srcs = [
          (src + "/level0/package.lisp")
          (src + "/level0/impl.lisp")
          (src + "/level1/package.lisp")
          (src + "/level1/impl.lisp")
          (src + "/level2/package.lisp")
          (src + "/level2/impl.lisp")
          (src + "/level2/sugars.lisp")
          (src + "/level2/derived.lisp")
          (src + "/level2/derived-class.lisp")
          (src + "/level2/derived2.lisp")
          (src + "/level2/derived3.lisp")
          (src + "/level2/derived-numbers.lisp")
          (src + "/level2/arrays.lisp")
          (src + "/level2/inline-pattern.lisp")
        ];
      };

    fare-quasiquote = let src = pkgs.srcOnly pkgs.sbcl.pkgs.fare-quasiquote; in
      buildLisp.library {
        name = "fare-quasiquote";
        deps = [ fare-utils ];
        srcs = map (f: src + ("/" + f)) [
          "packages.lisp"
          "quasiquote.lisp"
          "pp-quasiquote.lisp"
        ];
      };

    usocket = let src = pkgs.srcOnly pkgs.sbcl.pkgs.usocket; in
      buildLisp.library {
        name = "usocket";
        deps = [ asdf sb-bsd-sockets split-sequence bordeaux-threads ];
        srcs = [
          (builtins.toFile "usocket.asd" ''
            (in-package :asdf)
            (defsystem usocket
              :version "0.8.8")
          '')
        ] ++ map (f: src + ("/" + f)) [
          "package.lisp"
          "usocket.lisp"
          "condition.lisp"
        ] ++ [
          "${src}/backend/sbcl.lisp"
          "${src}/option.lisp"
          "${src}/server.lisp"
        ];
      };

    # --- New from core ---

    ironclad = let
      src = pkgs.srcOnly pkgs.sbcl.pkgs.ironclad;
      getSrc = f: "${src}/src/${f}";
    in
      buildLisp.library {
        name = "ironclad";
        deps = [
          asdf
          sb-rotate-byte
          sb-posix
          alexandria
          bordeaux-threads
          nibbles
        ];
        srcs = map getSrc [
          "package.lisp"
          "conditions.lisp"
          "generic.lisp"
          "macro-utils.lisp"
          "util.lisp"
          # SBCL-specific optimizations (cl-deps is SBCL-only)
          "opt/sbcl/fndb.lisp"
          "opt/sbcl/x86oid-vm.lisp"
          "opt/sbcl/cpu-features.lisp"
          "common.lisp"
          "ciphers/cipher.lisp"
          "ciphers/padding.lisp"
          "ciphers/make-cipher.lisp"
          "ciphers/modes.lisp"
          "digests/digest.lisp"
          "macs/mac.lisp"
          "prng/prng.lisp"
          "prng/os-prng.lisp"
          "math.lisp"
          "octet-stream.lisp"
          "aead/aead.lisp"
          "kdf/kdf.lisp"
          "public-key/public-key.lisp"
          "public-key/pkcs1.lisp"
          "public-key/elliptic-curve.lisp"
          "ciphers/aes.lisp"
          "ciphers/arcfour.lisp"
          "ciphers/aria.lisp"
          "ciphers/blowfish.lisp"
          "ciphers/camellia.lisp"
          "ciphers/cast5.lisp"
          "ciphers/chacha.lisp"
          "ciphers/des.lisp"
          "ciphers/idea.lisp"
          "ciphers/kalyna.lisp"
          "ciphers/kuznyechik.lisp"
          "ciphers/misty1.lisp"
          "ciphers/rc2.lisp"
          "ciphers/rc5.lisp"
          "ciphers/rc6.lisp"
          "ciphers/salsa20.lisp"
          "ciphers/keystream.lisp"
          "ciphers/seed.lisp"
          "ciphers/serpent.lisp"
          "ciphers/sm4.lisp"
          "ciphers/sosemanuk.lisp"
          "ciphers/square.lisp"
          "ciphers/tea.lisp"
          "ciphers/threefish.lisp"
          "ciphers/twofish.lisp"
          "ciphers/xchacha.lisp"
          "ciphers/xor.lisp"
          "ciphers/xsalsa20.lisp"
          "ciphers/xtea.lisp"
          "digests/adler32.lisp"
          "digests/blake2.lisp"
          "digests/blake2s.lisp"
          "digests/crc24.lisp"
          "digests/crc32.lisp"
          "digests/groestl.lisp"
          "digests/jh.lisp"
          "digests/kupyna.lisp"
          "digests/md2.lisp"
          "digests/md4.lisp"
          "digests/md5.lisp"
          "digests/md5-lispworks-int32.lisp"
          "digests/ripemd-128.lisp"
          "digests/ripemd-160.lisp"
          "digests/sha1.lisp"
          "digests/sha256.lisp"
          "digests/sha3.lisp"
          "digests/sha512.lisp"
          "digests/skein.lisp"
          "digests/sm3.lisp"
          "digests/streebog.lisp"
          "digests/tiger.lisp"
          "digests/tree-hash.lisp"
          "digests/whirlpool.lisp"
          "macs/blake2-mac.lisp"
          "macs/blake2s-mac.lisp"
          "macs/cmac.lisp"
          "macs/hmac.lisp"
          "macs/gmac.lisp"
          "macs/poly1305.lisp"
          "macs/siphash.lisp"
          "macs/skein-mac.lisp"
          "prng/generator.lisp"
          "prng/fortuna.lisp"
          "aead/eax.lisp"
          "aead/etm.lisp"
          "aead/gcm.lisp"
          "kdf/argon2.lisp"
          "kdf/bcrypt.lisp"
          "kdf/hmac.lisp"
          "kdf/pkcs5.lisp"
          "kdf/password-hash.lisp"
          "kdf/scrypt.lisp"
          "public-key/dsa.lisp"
          "public-key/rsa.lisp"
          "public-key/elgamal.lisp"
          "public-key/curve25519.lisp"
          "public-key/curve448.lisp"
          "public-key/ed25519.lisp"
          "public-key/ed448.lisp"
          "public-key/secp256k1.lisp"
          "public-key/secp256r1.lisp"
          "public-key/secp384r1.lisp"
          "public-key/secp521r1.lisp"
        ];
      };

    parenscript = let src = pkgs.srcOnly pkgs.sbcl.pkgs.parenscript; in
      buildLisp.library {
        name = "parenscript";
        deps = [ cl-ppcre anaphora named-readtables ];
        srcs = [
          (src + "/src/package.lisp")
          (src + "/src/js-dom-symbol-exports.lisp")
          (src + "/src/js-ir-package.lisp")
          (src + "/src/namespace.lisp")
          (src + "/src/compiler.lisp")
          (src + "/src/printer.lisp")
          (src + "/src/compilation-interface.lisp")
          (src + "/src/utils.lisp")
          (src + "/src/non-cl.lisp")
          (src + "/src/special-operators.lisp")
          (src + "/src/parse-lambda-list.lisp")
          (src + "/src/function-definition.lisp")
          (src + "/src/macros.lisp")
          (src + "/src/deprecated-interface.lisp")
          (src + "/src/lib/ps-html.lisp")
          (src + "/src/lib/ps-loop.lisp")
          (src + "/src/lib/ps-dom.lisp")
          (src + "/runtime/ps-runtime-lib.lisp")
        ];
      };

    cl-json = let
      src = pkgs.fetchFromGitHub {
        owner = "sternenseemann";
        repo = "cl-json";
        rev = "c059bec94e28a11102a994d6949e2e52764f21fd";
        sha256 = "0l07syw1b1x2zi8kj4iph3rf6vi6c16b7fk69iv7x27wrdsr1qwj";
      };
      getSrcs = subdir: map (f: src + ("/" + subdir + "/" + f));
    in
      buildLisp.library {
        name = "cl-json";
        deps = [ asdf ];
        srcs = [ "${src}/cl-json.asd" ] ++
          (getSrcs "src" [
            "package.lisp"
            "common.lisp"
            "objects.lisp"
            "camel-case.lisp"
            "decoder.lisp"
            "encoder.lisp"
            "utils.lisp"
            "json-rpc.lisp"
          ]);
      };

    # ===================================================================
    # Tier 3: three levels deep
    # ===================================================================

    static-vectors = let src = pkgs.fetchFromGitHub {
      owner = "sionescu"; repo = "static-vectors";
      rev = "v1.8.9";
      sha256 = "sha256-3BGtfPZH4qJKrZ6tJxf18QMbkn4qEofD198qSIFQOB0=";
    }; in
      buildLisp.library {
        name = "static-vectors";
        deps = [ alexandria cffi ];
        srcs = [
          "${src}/src/pkgdcl.lisp"
          "${src}/src/constantp.lisp"
          "${src}/src/impl-sbcl.lisp"
          "${src}/src/constructor.lisp"
          "${src}/src/cffi-type-translator.lisp"
        ];
      };

    smart-buffer = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "smart-buffer";
      rev = "master";
      sha256 = "sha256-+d58K2b6y8umupE3Yw9Hxw/DqEG6R/EeVqeGdFQwPuU=";
    }; in
      buildLisp.library {
        name = "smart-buffer";
        deps = [ xsubseq flexi-streams uiop ];
        srcs = [ "${src}/src/smart-buffer.lisp" ];
      };

    local-time = let src = pkgs.srcOnly pkgs.sbcl.pkgs.local-time; in
      buildLisp.library {
        name = "local-time";
        deps = [ cl-fad uiop ];
        srcs = [
          "${src}/src/package.lisp"
          "${src}/src/local-time.lisp"
        ];
      };

    fare-quasiquote-readtable = let src = pkgs.srcOnly pkgs.sbcl.pkgs.fare-quasiquote; in
      buildLisp.library {
        name = "fare-quasiquote-readtable";
        deps = [ named-readtables fare-quasiquote ];
        srcs = [ (src + "/quasiquote-readtable.lisp") ];
      };

    cl-plus-ssl = let src = pkgs.srcOnly pkgs.sbcl.pkgs.cl_plus_ssl; in
      buildLisp.library {
        name = "cl-plus-ssl";
        deps = [
          alexandria bordeaux-threads cffi flexi-streams
          trivial-features trivial-garbage trivial-gray-streams
          usocket uiop sb-posix
        ];
        native = [ pkgs.openssl ];
        srcs = map (f: src + ("/src/" + f)) [
          "config.lisp"
          "package.lisp"
          "reload.lisp"
          "ffi.lisp"
          "bio.lisp"
          "conditions.lisp"
          "ssl-funcall.lisp"
          "init.lisp"
          "ffi-buffer-all.lisp"
          "ffi-buffer.lisp"
          "streams.lisp"
          "x509.lisp"
          "random.lisp"
          "context.lisp"
          "verify-hostname.lisp"
        ];
      };

    # --- New from core ---

    circular-streams = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "circular-streams";
      rev = "master";
      sha256 = "sha256-OpeLjFbiiwycwZjMeYgu7YoyFYy7HieSY9hHxkoz/PI=";
    }; in
      buildLisp.library {
        name = "circular-streams";
        deps = [ fast-io trivial-gray-streams ];
        srcs = [ "${src}/src/circular-streams.lisp" ];
      };

    trivial-rfc-1123 = let src = pkgs.fetchFromGitHub {
      owner = "stacksmith"; repo = "trivial-rfc-1123";
      rev = "master";
      sha256 = "sha256-3cCwIsm8Wd2jq/YKey8l01v0aKe/CaM8O9c6EOTlnvA=";
    }; in
      buildLisp.library {
        name = "trivial-rfc-1123";
        deps = [ cl-ppcre ];
        srcs = [
          "${src}/package.lisp"
          "${src}/trivial-rfc-1123.lisp"
        ];
      };

    # ===================================================================
    # Tier 4: four levels deep
    # ===================================================================

    fast-io = let src = pkgs.fetchFromGitHub {
      owner = "rpav"; repo = "fast-io";
      rev = "master";
      sha256 = "sha256-YBTROnJyB8w3H+GDhlHI+6n7XvnyoGN+8lDh9ZQXAHI=";
    }; in
      buildLisp.library {
        name = "fast-io";
        deps = [ alexandria trivial-gray-streams static-vectors ];
        srcs = [
          "${src}/src/package.lisp"
          "${src}/src/types.lisp"
          "${src}/src/io.lisp"
          "${src}/src/gray.lisp"
        ];
      };

    fast-http = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "fast-http";
      rev = "master";
      sha256 = "sha256-AD4fOVh5dlLJGYLlO16prkkpNhJF95zV9zDRQISAnRE=";
    }; in
      buildLisp.library {
        name = "fast-http";
        deps = [ alexandria cl-utilities babel proc-parse xsubseq smart-buffer ];
        srcs = [
          "${src}/src/byte-vector.lisp"
          "${src}/src/error.lisp"
          "${src}/src/util.lisp"
          "${src}/src/http.lisp"
          "${src}/src/parser.lisp"
          "${src}/src/multipart-parser.lisp"
          "${src}/src/fast-http.lisp"
        ];
      };

    quri = let src = pkgs.srcOnly pkgs.sbcl.pkgs.quri; in
      buildLisp.library {
        name = "quri";
        deps = [ babel alexandria split-sequence cl-utilities idna sb-cltl2 uiop ];
        srcs = [
          "${src}/src/error.lisp"
          "${src}/src/util.lisp"
          "${src}/src/port.lisp"
          (pkgs.runCommand "etld.lisp" { } ''
            substitute "${src}/src/etld.lisp" "$out" \
              --replace-fail \
                ${lib.escapeShellArg "#.(asdf:system-relative-pathname :quri #P\"data/effective_tld_names.dat\")"} \
                '"${src}/data/effective_tld_names.dat"'
          '')
          "${src}/src/encode.lisp"
          "${src}/src/decode.lisp"
          "${src}/src/parser.lisp"
          "${src}/src/uri.lisp"
          "${src}/src/uri/ftp.lisp"
          "${src}/src/uri/http.lisp"
          "${src}/src/uri/ldap.lisp"
          "${src}/src/uri/file.lisp"
          "${src}/src/domain.lisp"
          "${src}/src/quri.lisp"
        ];
      };

    trivia-quasiquote = let src = pkgs.sbcl.pkgs.trivia.src; in
      buildLisp.library {
        name = "trivia-quasiquote";
        deps = [ fare-quasiquote-readtable trivia ];
        srcs = [ (src + "/quasiquote/quasiquote.lisp") ];
      };

    cl-cookie = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "cl-cookie";
      rev = "cea55aed8b9ad25fafd13defbcb9fe8f41b29546";
      sha256 = "sha256-EFHefcWjFDw5k2BwUSVsBgFiJYEb9EYD2x8lWM4/DyQ=";
    }; in
      buildLisp.library {
        name = "cl-cookie";
        deps = [ proc-parse cl-ppcre quri local-time alexandria ];
        srcs = [ "${src}/src/cl-cookie.lisp" ];
      };

    # --- New from core ---

    http-body = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "http-body";
      rev = "master";
      sha256 = "sha256-5C8/tNp9W5De7Hs5cMoxN9o1ffuRnEzivdC/eE5UpFw=";
    }; in
      buildLisp.library {
        name = "http-body";
        deps = [
          fast-http yason trivial-gray-streams
          quri flexi-streams babel cl-ppcre cl-utilities
        ];
        srcs = [
          "${src}/src/util.lisp"
          "${src}/src/urlencoded.lisp"
          "${src}/src/json.lisp"
          "${src}/src/multipart.lisp"
          "${src}/src/http-body.lisp"
        ];
      };

    fast-websocket = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "fast-websocket";
      rev = "master";
      sha256 = "sha256-Xw6vlbKdGu5XPGY9D13Ty0al3lWGTQT6pcakIxoqX4A=";
    }; in
      buildLisp.library {
        name = "fast-websocket";
        deps = [ fast-io babel alexandria ];
        srcs = [
          "${src}/src/error.lisp"
          "${src}/src/constants.lisp"
          "${src}/src/payload.lisp"
          "${src}/src/compose.lisp"
          "${src}/src/ws.lisp"
          "${src}/src/parser.lisp"
          "${src}/src/fast-websocket.lisp"
        ];
      };

    # ===================================================================
    # Tier 5: top-level direct deps
    # ===================================================================

    yason = let src = pkgs.sbcl.pkgs.yason.src; in
      buildLisp.library {
        name = "yason";
        deps = [ alexandria trivial-gray-streams ];
        srcs = map (f: src + ("/" + f)) [
          "package.lisp"
          "parse.lisp"
          "encode.lisp"
        ];
      };

    let-over-lambda =
      buildLisp.library {
        name = "let-over-lambda";
        deps = [ alexandria cl-ppcre named-readtables fare-quasiquote-readtable trivia-quasiquote ];
        srcs = [
          ./let-over-lambda/package.lisp
          ./let-over-lambda/let-over-lambda.lisp
        ];
      };

    hunchentoot = let
      src = pkgs.srcOnly pkgs.sbcl.pkgs.hunchentoot;
      url-rewrite = buildLisp.library {
        name = "url-rewrite";
        srcs = map (f: src + ("/url-rewrite/" + f)) [
          "packages.lisp"
          "specials.lisp"
          "primitives.lisp"
          "util.lisp"
          "url-rewrite.lisp"
        ];
      };
    in
      buildLisp.library {
        name = "hunchentoot";
        deps = [
          alexandria bordeaux-threads chunga cl-base64 cl-fad
          rfc2388 cl-plus-ssl cl-ppcre flexi-streams md5
          trivial-backtrace usocket url-rewrite
        ];
        srcs = map (f: src + ("/" + f)) [
          "packages.lisp"
          "compat.lisp"
        ] ++ [
          (pkgs.runCommand "specials.lisp" { } ''
            substitute "${src}/specials.lisp" "$out" --replace-fail \
              ${lib.escapeShellArg "#.(asdf:component-version (asdf:find-system :hunchentoot))"} \
              '"${lib.removePrefix "v" src.version}"'
          '')
        ] ++ map (f: src + ("/" + f)) [
          "conditions.lisp"
          "mime-types.lisp"
          "util.lisp"
          "log.lisp"
          "cookie.lisp"
          "reply.lisp"
          "request.lisp"
          "session.lisp"
          "misc.lisp"
          "headers.lisp"
          "set-timeouts.lisp"
          "taskmaster.lisp"
          "ssl.lisp"
          "acceptor.lisp"
          "easy-handlers.lisp"
        ];
      };

    dexador = let
      version = "0.9.15";
      src = pkgs.fetchFromGitHub {
        owner = "fukamachi"; repo = "dexador";
        rev = "master";
        sha256 = "sha256-ccl+AAlCC7wsAVLNgFRhDEC946M43om6W7tKU2hCJh0=";
      };
    in
      buildLisp.library {
        name = "dexador";
        deps = [
          fast-http quri fast-io babel trivial-gray-streams trivial-garbage
          chunga cl-ppcre cl-cookie trivial-mimes chipz cl-base64
          usocket cl-plus-ssl bordeaux-threads alexandria uiop
        ];
        srcs = [
          "${src}/src/encoding.lisp"
          (pkgs.runCommand "util.lisp" { } ''
            substitute "${src}/src/util.lisp" "$out" --replace-fail \
              ${lib.escapeShellArg "(asdf:component-version (asdf:find-system :dexador))"} \
              '"${version}"'
          '')
        ] ++ map (f: "${src}/src/${f}") [
          "connection-cache.lisp"
          "decoding-stream.lisp"
          "keep-alive-stream.lisp"
          "body.lisp"
          "error.lisp"
          "restarts.lisp"
          "backend/usocket.lisp"
          "dexador.lisp"
        ];
      };

    # ===================================================================
    # Clack/Lack ecosystem (new from core)
    # ===================================================================

    lack-component = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-component";
        srcs = [ "${lackSrc}/src/component.lisp" ];
      };

    lack-util = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-util";
        deps = [ ironclad ];
        srcs = [ "${lackSrc}/src/util.lisp" ];
      };

    lack-request = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-request";
        deps = [ quri cl-ppcre http-body circular-streams ];
        srcs = [
          "${lackSrc}/src/media-type.lisp"
          "${lackSrc}/src/request.lisp"
        ];
      };

    lack-response = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-response";
        deps = [ quri local-time ];
        srcs = [ "${lackSrc}/src/response.lisp" ];
      };

    lack-app-file = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-app-file";
        deps = [ alexandria trivial-mimes trivial-rfc-1123 lack-component uiop ];
        srcs = [ "${lackSrc}/src/app/file.lisp" ];
      };

    lack-middleware-backtrace = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-middleware-backtrace";
        deps = [ uiop ];
        srcs = [ "${lackSrc}/src/middleware/backtrace.lisp" ];
      };

    lack-middleware-session = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-middleware-session";
        deps = [ bordeaux-threads cl-ppcre lack-request lack-response lack-util ];
        srcs = [
          "${lackSrc}/src/middleware/session/store.lisp"
          "${lackSrc}/src/middleware/session/store/memory.lisp"
          "${lackSrc}/src/middleware/session/state.lisp"
          "${lackSrc}/src/middleware/session/state/cookie.lisp"
          "${lackSrc}/src/middleware/session.lisp"
        ];
      };

    lack-middleware-csrf = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-middleware-csrf";
        deps = [ lack-request lack-util ];
        srcs = [ "${lackSrc}/src/middleware/csrf.lisp" ];
      };

    lack-middleware-static = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-middleware-static";
        deps = [ alexandria lack-app-file lack-component ];
        srcs = [ "${lackSrc}/src/middleware/static.lisp" ];
      };

    lack-middleware-accesslog = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack-middleware-accesslog";
        deps = [ local-time lack-util ];
        srcs = [ "${lackSrc}/src/middleware/accesslog.lisp" ];
      };

    lack = let lackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "lack";
      rev = "master";
      sha256 = "sha256-588UafCp5t5nX09YyQc0MrV6WGalLrbDyZRqqpyyX7U=";
    }; in
      buildLisp.library {
        name = "lack";
        deps = [ lack-component lack-util ];
        srcs = [
          "${lackSrc}/src/builder.lisp"
          "${lackSrc}/src/lack.lisp"
        ];
      };

    clack = let
      clackSrc = pkgs.fetchFromGitHub {
        owner = "fukamachi"; repo = "clack";
        rev = "master";
        sha256 = "sha256-hKTmQzrBTAonzkUr/iwA54oAEPDwyoGpiQ/koSwLy6w=";
      };
      swankStub = pkgs.writeText "swank-stub.lisp" ''
        ;; Only define stub if real Swank is not already loaded
        (unless (find-package :swank)
          (defpackage :swank
            (:use :cl)
            (:export :create-server :stop-server))
          (in-package :swank)
          (defvar *swank-available* nil)
          (defun create-server (&rest args)
            (declare (ignore args))
            (warn "SWANK not available - install full swank for REPL support")
            nil)
          (defun stop-server (&rest args)
            (declare (ignore args))
            nil))
      '';
    in
      buildLisp.library {
        name = "clack";
        deps = [
          alexandria bordeaux-threads usocket
          lack lack-middleware-backtrace lack-util uiop
        ];
        srcs = [
          swankStub
          "${clackSrc}/src/util.lisp"
          "${clackSrc}/src/socket.lisp"
          "${clackSrc}/src/handler.lisp"
          "${clackSrc}/src/clack.lisp"
        ];
      };

    clack-handler-hunchentoot = let clackSrc = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "clack";
      rev = "master";
      sha256 = "sha256-hKTmQzrBTAonzkUr/iwA54oAEPDwyoGpiQ/koSwLy6w=";
    }; in
      buildLisp.library {
        name = "clack-handler-hunchentoot";
        deps = [
          hunchentoot alexandria bordeaux-threads
          flexi-streams split-sequence
          clack lack-request
        ];
        srcs = [
          "${clackSrc}/src/handler/hunchentoot.lisp"
        ];
      };

    # ===================================================================
    # WebSocket Driver (new from core)
    # ===================================================================

    websocket-driver-base = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "websocket-driver";
      rev = "master";
      sha256 = "sha256-pANz4bitab8ByV5WDBf7eEoTYRvfQRyOiq2Sv0+4cec=";
    }; in
      buildLisp.library {
        name = "websocket-driver-base";
        deps = [
          fast-websocket fast-io event-emitter sha1
          cl-base64 split-sequence bordeaux-threads
        ];
        srcs = [
          "${src}/src/util.lisp"
          "${src}/src/ws/base.lisp"
          "${src}/src/driver.lisp"
        ];
      };

    websocket-driver-server = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "websocket-driver";
      rev = "master";
      sha256 = "sha256-pANz4bitab8ByV5WDBf7eEoTYRvfQRyOiq2Sv0+4cec=";
    }; in
      buildLisp.library {
        name = "websocket-driver-server";
        deps = [
          fast-websocket fast-io babel
          websocket-driver-base clack
        ];
        srcs = [
          "${src}/src/ws/server.lisp"
          "${src}/src/server.lisp"
        ];
      };

    websocket-driver-client = let src = pkgs.fetchFromGitHub {
      owner = "fukamachi"; repo = "websocket-driver";
      rev = "master";
      sha256 = "sha256-pANz4bitab8ByV5WDBf7eEoTYRvfQRyOiq2Sv0+4cec=";
    }; in
      buildLisp.library {
        name = "websocket-driver-client";
        deps = [
          usocket cl-plus-ssl fast-io fast-websocket
          fast-http cl-base64 babel quri
          websocket-driver-base
        ];
        srcs = [
          "${src}/src/ws/client.lisp"
          "${src}/src/client.lisp"
        ];
      };

    # ===================================================================
    # Swank — SLIME's Common Lisp server
    # ===================================================================

    swank = let
      slimeSrc = pkgs.fetchFromGitHub {
        owner = "slime";
        repo = "slime";
        rev = "v2.30";
        sha256 = "0qb7m65gq0mbxfrdppkh3k4jn13i14i07ziga4r8b3rmrxhrmlv0";
      };
      patchedSlimeSrc = pkgs.applyPatches {
        name = "slime-source-patched";
        src = slimeSrc;
        patches = [ ./swank-compile-file-fix.patch ];
      };
      src = f: "${patchedSlimeSrc}/${f}";
      packageLoader = pkgs.writeText "swank-package-loader.lisp" ''
        ;;; Package definitions for Swank (replaces swank-loader:define-package)
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (unless (find-package :swank)
            (make-package :swank :use '(:cl)))
          (flet ((forward-declare-var (name)
                   (let ((sym (intern name :swank)))
                     (unless (boundp sym)
                       (proclaim `(special ,sym))
                       (setf (symbol-value sym) nil))
                     (export sym :swank)))
                 (forward-declare-fn (name)
                   (let ((sym (intern name :swank)))
                     (unless (fboundp sym)
                       (setf (fdefinition sym)
                             (lambda (&rest args)
                               (declare (ignore args))
                               (error "~A not yet defined" sym))))
                     (export sym :swank))))
            (forward-declare-var "*COMMUNICATION-STYLE*")
            (forward-declare-var "*SWANK-DEBUGGER-CONDITION*")
            (forward-declare-fn "SWANK-DEBUGGER-HOOK")
            (forward-declare-fn "Y-OR-N-P-IN-EMACS")))

        (defpackage :swank-loader
          (:use :cl)
          (:export :define-package
                   :init
                   :*source-directory*
                   :*fasl-directory*))

        (in-package :swank-loader)

        (defvar *source-directory* nil)
        (defvar *fasl-directory* nil)

        (defmacro define-package (name &rest options)
          "Define a package like DEFPACKAGE but with a few extensions.
        Supports :IMPORT-FROM with no symbol list to import all external symbols."
          (let* ((import-from (loop for (key . args) in options
                                    when (eq key :import-from)
                                    collect args))
                 (other-options (remove :import-from options :key #'car)))
            `(defpackage ,name
               ,@other-options
               ,@(loop for (pkg . syms) in import-from
                       collect `(:import-from ,pkg ,@syms)))))

        (defun init (&key load-contribs)
          "No-op init function for compatibility."
          (declare (ignore load-contribs))
          t)
      '';
    in
      buildLisp.library {
        name = "swank";
        deps = [ bordeaux-threads ];
        srcs = [
          packageLoader
          (src "packages.lisp")
          (src "swank/backend.lisp")
          (src "swank/rpc.lisp")
          (src "swank/match.lisp")
          (src "swank/source-path-parser.lisp")
          (src "swank/source-file-cache.lisp")
          (src "swank/sbcl.lisp")
          (src "swank/gray.lisp")
          (src "swank.lisp")
          (src "contrib/swank-util.lisp")
          (src "contrib/swank-c-p-c.lisp")
          (src "contrib/swank-arglists.lisp")
          (src "contrib/swank-fuzzy.lisp")
        ];
      };

    # ===================================================================
    # Test-only deps
    # ===================================================================

    asdf-flv =
      buildLisp.library {
        name = "asdf-flv";
        deps = [ asdf ];
        srcs = [
          ./asdf-flv/package.lisp
          ./asdf-flv/asdf-flv.lisp
        ];
      };

    fiveam = let src = pkgs.srcOnly pkgs.sbcl.pkgs.fiveam; in
      buildLisp.library {
        name = "fiveam";
        deps = [ alexandria asdf-flv trivial-backtrace ];
        srcs = map (f: src + ("/src/" + f)) [
          "package.lisp"
          "utils.lisp"
          "check.lisp"
          "fixture.lisp"
          "classes.lisp"
          "random.lisp"
          "test.lisp"
          "explain.lisp"
          "suite.lisp"
          "run.lisp"
        ];
      };
  };

in { inherit buildLisp lisp; }
