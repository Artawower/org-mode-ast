import { parse } from 'parser/parser';
import { hasNodeIncorrectRanges } from 'test-helper';
import { KNOWN_ENTITIES } from 'tokenizer/entities.const';

describe('Entities', () => {
  it('Should find entity at the start of org doc', () => {
    let res = '';
    KNOWN_ENTITIES.forEach((e) => {
      const orgDoc = `\\${e} - it's entity`;
      const result = parse(orgDoc);
      res += result.toString() + '------------------\n';
      expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    });
    expect(res).toMatchInlineSnapshot(`
      "root [0-21]
        entity [0-7] ("\\\\Agrave")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\agrave")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Aacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\aacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Acirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\acirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Amacr")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\amacr")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Atilde")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\atilde")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Auml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\auml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Aring")
        text [6-20] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\AA")
        text [3-17] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\aring")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\AElig")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\aelig")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Ccedil")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\ccedil")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Egrave")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\egrave")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Eacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\eacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Ecirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\ecirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Euml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\euml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Igrave")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\igrave")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Iacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\iacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Idot")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-3] ("\\\\in")
        text [3-21] ("odot - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Icirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\icirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Iuml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\iuml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Ntilde")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\ntilde")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Ograve")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\ograve")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Oacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\oacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Ocirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\ocirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Otilde")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\otilde")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Ouml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\ouml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Oslash")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\oslash")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\OElig")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\oelig")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-2] ("\\\\S")
        text [2-21] ("caron - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\scaron")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\szlig")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Ugrave")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\ugrave")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Uacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\uacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Ucirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\ucirc")
        text [6-20] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Uuml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\uuml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Yacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\yacute")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Yuml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\yuml")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\fnof")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\real")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\image")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\weierp")
        text [7-21] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\ell")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\imath")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\jmath")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\GREEK")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Alpha")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\alpha")
        text [6-20] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Beta")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\beta")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Gamma")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\gamma")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Delta")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\delta")
        text [6-20] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\Epsilon")
        text [8-22] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\epsilon")
        text [8-22] (" - it's entity")
      ------------------
      root [0-25]
        entity [0-11] ("\\\\varepsilon")
        text [11-25] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Zeta")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\zeta")
        text [5-19] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\Eta")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\eta")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Theta")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\theta")
        text [6-20] (" - it's entity")
      ------------------
      root [0-23]
        entity [0-6] ("\\\\theta")
        text [6-23] ("sym - it's entity")
      ------------------
      root [0-23]
        entity [0-9] ("\\\\vartheta")
        text [9-23] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\Iota")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\iota")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Kappa")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\kappa")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Lambda")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\lambda")
        text [7-21] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\Mu")
        text [3-17] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\mu")
        text [3-17] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\nu")
        text [3-17] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\Nu")
        text [3-17] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\Xi")
        text [3-17] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\xi")
        text [3-17] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\Omicron")
        text [8-22] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\omicron")
        text [8-22] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\Pi")
        text [3-17] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\pi")
        text [3-17] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\Rho")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\rho")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-2] ("\\\\S")
        text [2-20] ("igma - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\sigma")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-6] ("\\\\sigma")
        text [6-21] ("f - it's entity")
      ------------------
      root [0-23]
        entity [0-9] ("\\\\varsigma")
        text [9-23] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\Tau")
        text [4-18] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\Upsilon")
        text [8-22] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\upsih")
        text [6-20] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\upsilon")
        text [8-22] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\Phi")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\phi")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\varphi")
        text [7-21] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\Chi")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\chi")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-6] ("\\\\acute")
        text [6-21] ("x - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\Psi")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\psi")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\tau")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Omega")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\omega")
        text [6-20] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-3] ("\\\\pi")
        text [3-18] ("v - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\varpi")
        text [6-20] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\partial")
        text [8-22] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\HEBREW")
        text [7-21] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\alefsym")
        text [8-22] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\aleph")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\gimel")
        text [6-20] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\beth")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\dalet")
        text [6-20] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\ETH")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\eth")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\THORN")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\thorn")
        text [6-20] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\dots")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-5] ("\\\\cdot")
        text [5-20] ("s - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\hellip")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\middot")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\iexcl")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\iquest")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\DASH")
        text [5-19] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\shy")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\ndash")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\mdash")
        text [6-20] (" - it's entity")
      ------------------
      root [0-25]
        entity [0-11] ("\\\\QUOTATIONS")
        text [11-25] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\quot")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\acute")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\ldquo")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\rdquo")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\bdquo")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\lsquo")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\rsquo")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\sbquo")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\laquo")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\raquo")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\lsaquo")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\rsaquo")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\Other")
        text [6-20] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\MISC")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\circ")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\vert")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\vbar")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\brvbar")
        text [7-21] (" - it's entity")
      ------------------
      root [0-16]
        entity [0-2] ("\\\\S")
        text [2-16] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\sec")
        text [4-19] ("t - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\amp")
        text [4-18] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\lt")
        text [3-17] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\gt")
        text [3-17] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\tilde")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\slash")
        text [6-20] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\plus")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\under")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\equal")
        text [6-20] (" - it's entity")
      ------------------
      root [0-24]
        entity [0-10] ("\\\\asciicirc")
        text [10-24] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-4] ("\\\\dag")
        text [4-21] ("ger - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\dag")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\Dagger")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\ddag")
        text [5-19] (" - it's entity")
      ------------------
      root [0-25]
        entity [0-11] ("\\\\WHITESPACE")
        text [11-25] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\nbsp")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\ensp")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\emsp")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\thinsp")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\curren")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\cent")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\pound")
        text [6-20] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\yen")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\euro")
        text [5-19] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\EUR")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\dollar")
        text [7-21] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\USD")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\copy")
        text [5-19] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\reg")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\trade")
        text [6-20] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-2] ("\\\\S")
        text [2-22] ("CIENCE - it's entity")
      ------------------
      root [0-20]
        entity [0-4] ("\\\\min")
        text [4-20] ("us - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\pm")
        text [3-17] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-5] ("\\\\plus")
        text [5-21] ("mn - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\times")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\frasl")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\colon")
        text [6-20] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\div")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\frac12")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\frac14")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\frac34")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\permil")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\sup")
        text [4-19] ("1 - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\sup")
        text [4-19] ("2 - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\sup")
        text [4-19] ("3 - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\radic")
        text [6-20] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\sum")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\prod")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\micro")
        text [6-20] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\macr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\deg")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\prime")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-3] ("\\\\Pr")
        text [3-20] ("ime - it's entity")
      ------------------
      root [0-20]
        entity [0-3] ("\\\\in")
        text [3-20] ("fin - it's entity")
      ------------------
      root [0-20]
        entity [0-3] ("\\\\in")
        text [3-20] ("fty - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\prop")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-5] ("\\\\prop")
        text [5-21] ("to - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\not")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-3] ("\\\\ne")
        text [3-18] ("g - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\land")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\wedge")
        text [6-20] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\lor")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\vee")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\cap")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\cup")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\smile")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\frown")
        text [6-20] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-3] ("\\\\in")
        text [3-18] ("t - it's entity")
      ------------------
      root [0-24]
        entity [0-10] ("\\\\therefore")
        text [10-24] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\there4")
        text [7-21] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\because")
        text [8-22] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\sim")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\cong")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-4] ("\\\\sim")
        text [4-20] ("eq - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\asymp")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\approx")
        text [7-21] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\ne")
        text [3-17] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-3] ("\\\\ne")
        text [3-18] ("q - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\equiv")
        text [6-20] (" - it's entity")
      ------------------
      root [0-24]
        entity [0-10] ("\\\\triangleq")
        text [10-24] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\le")
        text [3-17] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-3] ("\\\\le")
        text [3-18] ("q - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\ge")
        text [3-17] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-3] ("\\\\ge")
        text [3-18] ("q - it's entity")
      ------------------
      root [0-22]
        entity [0-3] ("\\\\le")
        text [3-22] ("ssgtr - it's entity")
      ------------------
      root [0-24]
        entity [0-3] ("\\\\le")
        text [3-24] ("sseqgtr - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\ll")
        text [3-17] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\Ll")
        text [3-17] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-3] ("\\\\ll")
        text [3-18] ("l - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\gg")
        text [3-17] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\Gg")
        text [3-17] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-3] ("\\\\gg")
        text [3-18] ("g - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\prec")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-5] ("\\\\prec")
        text [5-21] ("eq - it's entity")
      ------------------
      root [0-26]
        entity [0-5] ("\\\\prec")
        text [5-26] ("curlyeq - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\succ")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-5] ("\\\\succ")
        text [5-21] ("eq - it's entity")
      ------------------
      root [0-26]
        entity [0-5] ("\\\\succ")
        text [5-26] ("curlyeq - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\sub")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-4] ("\\\\sub")
        text [4-21] ("set - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\sup")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-4] ("\\\\sup")
        text [4-21] ("set - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\nsub")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\sub")
        text [4-19] ("e - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\nsup")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\sup")
        text [4-19] ("e - it's entity")
      ------------------
      root [0-23]
        entity [0-9] ("\\\\setminus")
        text [9-23] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\forall")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\exist")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-6] ("\\\\exist")
        text [6-21] ("s - it's entity")
      ------------------
      root [0-21]
        entity [0-3] ("\\\\ne")
        text [3-21] ("xist - it's entity")
      ------------------
      root [0-22]
        entity [0-3] ("\\\\ne")
        text [3-22] ("xists - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\empty")
        text [6-20] (" - it's entity")
      ------------------
      root [0-23]
        entity [0-6] ("\\\\empty")
        text [6-23] ("set - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\isin")
        text [5-19] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\in")
        text [3-17] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-4] ("\\\\not")
        text [4-20] ("in - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\ni")
        text [3-17] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\nabla")
        text [6-20] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\ang")
        text [4-18] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-4] ("\\\\ang")
        text [4-20] ("le - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\perp")
        text [5-19] (" - it's entity")
      ------------------
      root [0-23]
        entity [0-5] ("\\\\para")
        text [5-23] ("llel - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\sdot")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\cdot")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\lceil")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\rceil")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\lfloor")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\rfloor")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\lang")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\rang")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-5] ("\\\\lang")
        text [5-21] ("le - it's entity")
      ------------------
      root [0-21]
        entity [0-5] ("\\\\rang")
        text [5-21] ("le - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\hbar")
        text [5-19] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\mho")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\ARROWS")
        text [7-21] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\larr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-24]
        entity [0-3] ("\\\\le")
        text [3-24] ("ftarrow - it's entity")
      ------------------
      root [0-19]
        entity [0-3] ("\\\\ge")
        text [3-19] ("ts - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\lArr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-24]
        entity [0-10] ("\\\\Leftarrow")
        text [10-24] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\uarr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\uparrow")
        text [8-22] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\uArr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\Uparrow")
        text [8-22] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\rarr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\to")
        text [3-17] (" - it's entity")
      ------------------
      root [0-25]
        entity [0-11] ("\\\\rightarrow")
        text [11-25] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\rArr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-25]
        entity [0-11] ("\\\\Rightarrow")
        text [11-25] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\darr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-24]
        entity [0-10] ("\\\\downarrow")
        text [10-24] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\dArr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-24]
        entity [0-10] ("\\\\Downarrow")
        text [10-24] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\harr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-29]
        entity [0-3] ("\\\\le")
        text [3-29] ("ftrightarrow - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\hArr")
        text [5-19] (" - it's entity")
      ------------------
      root [0-29]
        entity [0-15] ("\\\\Leftrightarrow")
        text [15-29] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\crarr")
        text [6-20] (" - it's entity")
      ------------------
      root [0-28]
        entity [0-14] ("\\\\hookleftarrow")
        text [14-28] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\arccos")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\arcsin")
        text [7-21] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\arctan")
        text [7-21] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\arg")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\cos")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\cos")
        text [4-19] ("h - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\cot")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\cot")
        text [4-19] ("h - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\csc")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\deg")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\det")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\dim")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\exp")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\gcd")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\hom")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-3] ("\\\\in")
        text [3-18] ("f - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\ker")
        text [4-18] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\lg")
        text [3-17] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\lim")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-4] ("\\\\lim")
        text [4-21] ("inf - it's entity")
      ------------------
      root [0-21]
        entity [0-4] ("\\\\lim")
        text [4-21] ("sup - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\ln")
        text [3-17] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\log")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\max")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\min")
        text [4-18] (" - it's entity")
      ------------------
      root [0-17]
        entity [0-3] ("\\\\Pr")
        text [3-17] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\sec")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\sin")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\sin")
        text [4-19] ("h - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\sup")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\tan")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-4] ("\\\\tan")
        text [4-19] ("h - it's entity")
      ------------------
      root [0-20]
        entity [0-2] ("\\\\S")
        text [2-20] ("IGNS - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\bull")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-5] ("\\\\bull")
        text [5-21] ("et - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\star")
        text [5-19] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\lowast")
        text [7-21] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\ast")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\odot")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\oplus")
        text [6-20] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\otimes")
        text [7-21] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\check")
        text [6-20] (" - it's entity")
      ------------------
      root [0-24]
        entity [0-6] ("\\\\check")
        text [6-24] ("mark - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\para")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\ordf")
        text [5-19] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\ordm")
        text [5-19] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\cedil")
        text [6-20] (" - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\oline")
        text [6-20] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\uml")
        text [4-18] (" - it's entity")
      ------------------
      root [0-19]
        entity [0-5] ("\\\\zwnj")
        text [5-19] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\zwj")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\lrm")
        text [4-18] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\rlm")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-6] ("\\\\smile")
        text [6-21] ("y - it's entity")
      ------------------
      root [0-25]
        entity [0-11] ("\\\\blacksmile")
        text [11-25] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\sad")
        text [4-18] (" - it's entity")
      ------------------
      root [0-21]
        entity [0-6] ("\\\\frown")
        text [6-21] ("y - it's entity")
      ------------------
      root [0-20]
        entity [0-2] ("\\\\S")
        text [2-20] ("UITS - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\clubs")
        text [6-20] (" - it's entity")
      ------------------
      root [0-23]
        entity [0-6] ("\\\\clubs")
        text [6-23] ("uit - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\spades")
        text [7-21] (" - it's entity")
      ------------------
      root [0-24]
        entity [0-7] ("\\\\spades")
        text [7-24] ("uit - it's entity")
      ------------------
      root [0-21]
        entity [0-7] ("\\\\hearts")
        text [7-21] (" - it's entity")
      ------------------
      root [0-24]
        entity [0-7] ("\\\\hearts")
        text [7-24] ("uit - it's entity")
      ------------------
      root [0-20]
        entity [0-6] ("\\\\diams")
        text [6-20] (" - it's entity")
      ------------------
      root [0-26]
        entity [0-8] ("\\\\diamond")
        text [8-26] ("suit - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\diamond")
        text [8-22] (" - it's entity")
      ------------------
      root [0-22]
        entity [0-8] ("\\\\Diamond")
        text [8-22] (" - it's entity")
      ------------------
      root [0-18]
        entity [0-4] ("\\\\loz")
        text [4-18] (" - it's entity")
      ------------------
      "
    `);
  });

  it('Should parse entity at the middle of text', () => {
    let res = '';
    KNOWN_ENTITIES.forEach((e) => {
      const orgDoc = `This is an entity - \\${e} !!`;
      const result = parse(orgDoc);
      res += result.toString() + '------------------\n';
      expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    });
    expect(res).toMatchInlineSnapshot(`
      "root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Agrave")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\agrave")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Aacute")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\aacute")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Acirc")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\acirc")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Amacr")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\amacr")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Atilde")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\atilde")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Auml")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\auml")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Aring")
        text [26-29] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\AA")
        text [23-26] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\aring")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\AElig")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\aelig")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Ccedil")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\ccedil")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Egrave")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\egrave")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Eacute")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\eacute")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Ecirc")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\ecirc")
        text [26-29] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Euml")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\euml")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Igrave")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\igrave")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Iacute")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\iacute")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Idot")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\in")
        text [23-30] ("odot !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Icirc")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\icirc")
        text [26-29] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Iuml")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\iuml")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Ntilde")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\ntilde")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Ograve")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\ograve")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Oacute")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\oacute")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Ocirc")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\ocirc")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Otilde")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\otilde")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Ouml")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\ouml")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Oslash")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\oslash")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\OElig")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\oelig")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-22] ("\\\\S")
        text [22-30] ("caron !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\scaron")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\szlig")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Ugrave")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\ugrave")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Uacute")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\uacute")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Ucirc")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\ucirc")
        text [26-29] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Uuml")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\uuml")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Yacute")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\yacute")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Yuml")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\yuml")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\fnof")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\real")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\image")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\weierp")
        text [27-30] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\ell")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\imath")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\jmath")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\GREEK")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Alpha")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\alpha")
        text [26-29] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Beta")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\beta")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Gamma")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\gamma")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Delta")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\delta")
        text [26-29] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\Epsilon")
        text [28-31] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\epsilon")
        text [28-31] (" !!")
      ------------------
      root [0-34]
        text [0-20] ("This is an entity - ")
        entity [20-31] ("\\\\varepsilon")
        text [31-34] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Zeta")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\zeta")
        text [25-28] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\Eta")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\eta")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Theta")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\theta")
        text [26-29] (" !!")
      ------------------
      root [0-32]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\theta")
        text [26-32] ("sym !!")
      ------------------
      root [0-32]
        text [0-20] ("This is an entity - ")
        entity [20-29] ("\\\\vartheta")
        text [29-32] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\Iota")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\iota")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Kappa")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\kappa")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Lambda")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\lambda")
        text [27-30] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\Mu")
        text [23-26] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\mu")
        text [23-26] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\nu")
        text [23-26] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\Nu")
        text [23-26] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\Xi")
        text [23-26] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\xi")
        text [23-26] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\Omicron")
        text [28-31] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\omicron")
        text [28-31] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\Pi")
        text [23-26] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\pi")
        text [23-26] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\Rho")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\rho")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-22] ("\\\\S")
        text [22-29] ("igma !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\sigma")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\sigma")
        text [26-30] ("f !!")
      ------------------
      root [0-32]
        text [0-20] ("This is an entity - ")
        entity [20-29] ("\\\\varsigma")
        text [29-32] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\Tau")
        text [24-27] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\Upsilon")
        text [28-31] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\upsih")
        text [26-29] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\upsilon")
        text [28-31] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\Phi")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\phi")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\varphi")
        text [27-30] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\Chi")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\chi")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\acute")
        text [26-30] ("x !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\Psi")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\psi")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\tau")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Omega")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\omega")
        text [26-29] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\pi")
        text [23-27] ("v !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\varpi")
        text [26-29] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\partial")
        text [28-31] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\HEBREW")
        text [27-30] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\alefsym")
        text [28-31] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\aleph")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\gimel")
        text [26-29] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\beth")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\dalet")
        text [26-29] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\ETH")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\eth")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\THORN")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\thorn")
        text [26-29] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\dots")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\cdot")
        text [25-29] ("s !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\hellip")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\middot")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\iexcl")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\iquest")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\DASH")
        text [25-28] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\shy")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\ndash")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\mdash")
        text [26-29] (" !!")
      ------------------
      root [0-34]
        text [0-20] ("This is an entity - ")
        entity [20-31] ("\\\\QUOTATIONS")
        text [31-34] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\quot")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\acute")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\ldquo")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\rdquo")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\bdquo")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\lsquo")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\rsquo")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\sbquo")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\laquo")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\raquo")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\lsaquo")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\rsaquo")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\Other")
        text [26-29] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\MISC")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\circ")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\vert")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\vbar")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\brvbar")
        text [27-30] (" !!")
      ------------------
      root [0-25]
        text [0-20] ("This is an entity - ")
        entity [20-22] ("\\\\S")
        text [22-25] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sec")
        text [24-28] ("t !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\amp")
        text [24-27] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\lt")
        text [23-26] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\gt")
        text [23-26] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\tilde")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\slash")
        text [26-29] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\plus")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\under")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\equal")
        text [26-29] (" !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-30] ("\\\\asciicirc")
        text [30-33] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\dag")
        text [24-30] ("ger !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\dag")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\Dagger")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\ddag")
        text [25-28] (" !!")
      ------------------
      root [0-34]
        text [0-20] ("This is an entity - ")
        entity [20-31] ("\\\\WHITESPACE")
        text [31-34] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\nbsp")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\ensp")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\emsp")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\thinsp")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\curren")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\cent")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\pound")
        text [26-29] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\yen")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\euro")
        text [25-28] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\EUR")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\dollar")
        text [27-30] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\USD")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\copy")
        text [25-28] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\reg")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\trade")
        text [26-29] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-22] ("\\\\S")
        text [22-31] ("CIENCE !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\min")
        text [24-29] ("us !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\pm")
        text [23-26] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\plus")
        text [25-30] ("mn !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\times")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\frasl")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\colon")
        text [26-29] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\div")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\frac12")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\frac14")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\frac34")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\permil")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sup")
        text [24-28] ("1 !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sup")
        text [24-28] ("2 !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sup")
        text [24-28] ("3 !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\radic")
        text [26-29] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sum")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\prod")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\micro")
        text [26-29] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\macr")
        text [25-28] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\deg")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\prime")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\Pr")
        text [23-29] ("ime !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\in")
        text [23-29] ("fin !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\in")
        text [23-29] ("fty !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\prop")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\prop")
        text [25-30] ("to !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\not")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ne")
        text [23-27] ("g !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\land")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\wedge")
        text [26-29] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\lor")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\vee")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\cap")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\cup")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\smile")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\frown")
        text [26-29] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\in")
        text [23-27] ("t !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-30] ("\\\\therefore")
        text [30-33] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\there4")
        text [27-30] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\because")
        text [28-31] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sim")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\cong")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sim")
        text [24-29] ("eq !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\asymp")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\approx")
        text [27-30] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ne")
        text [23-26] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ne")
        text [23-27] ("q !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\equiv")
        text [26-29] (" !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-30] ("\\\\triangleq")
        text [30-33] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\le")
        text [23-26] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\le")
        text [23-27] ("q !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ge")
        text [23-26] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ge")
        text [23-27] ("q !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\le")
        text [23-31] ("ssgtr !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\le")
        text [23-33] ("sseqgtr !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ll")
        text [23-26] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\Ll")
        text [23-26] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ll")
        text [23-27] ("l !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\gg")
        text [23-26] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\Gg")
        text [23-26] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\gg")
        text [23-27] ("g !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\prec")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\prec")
        text [25-30] ("eq !!")
      ------------------
      root [0-35]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\prec")
        text [25-35] ("curlyeq !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\succ")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\succ")
        text [25-30] ("eq !!")
      ------------------
      root [0-35]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\succ")
        text [25-35] ("curlyeq !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sub")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sub")
        text [24-30] ("set !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sup")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sup")
        text [24-30] ("set !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\nsub")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sub")
        text [24-28] ("e !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\nsup")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sup")
        text [24-28] ("e !!")
      ------------------
      root [0-32]
        text [0-20] ("This is an entity - ")
        entity [20-29] ("\\\\setminus")
        text [29-32] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\forall")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\exist")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\exist")
        text [26-30] ("s !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ne")
        text [23-30] ("xist !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ne")
        text [23-31] ("xists !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\empty")
        text [26-29] (" !!")
      ------------------
      root [0-32]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\empty")
        text [26-32] ("set !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\isin")
        text [25-28] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\in")
        text [23-26] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\not")
        text [24-29] ("in !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ni")
        text [23-26] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\nabla")
        text [26-29] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\ang")
        text [24-27] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\ang")
        text [24-29] ("le !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\perp")
        text [25-28] (" !!")
      ------------------
      root [0-32]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\para")
        text [25-32] ("llel !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\sdot")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\cdot")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\lceil")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\rceil")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\lfloor")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\rfloor")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\lang")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\rang")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\lang")
        text [25-30] ("le !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\rang")
        text [25-30] ("le !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\hbar")
        text [25-28] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\mho")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\ARROWS")
        text [27-30] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\larr")
        text [25-28] (" !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\le")
        text [23-33] ("ftarrow !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ge")
        text [23-28] ("ts !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\lArr")
        text [25-28] (" !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-30] ("\\\\Leftarrow")
        text [30-33] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\uarr")
        text [25-28] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\uparrow")
        text [28-31] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\uArr")
        text [25-28] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\Uparrow")
        text [28-31] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\rarr")
        text [25-28] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\to")
        text [23-26] (" !!")
      ------------------
      root [0-34]
        text [0-20] ("This is an entity - ")
        entity [20-31] ("\\\\rightarrow")
        text [31-34] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\rArr")
        text [25-28] (" !!")
      ------------------
      root [0-34]
        text [0-20] ("This is an entity - ")
        entity [20-31] ("\\\\Rightarrow")
        text [31-34] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\darr")
        text [25-28] (" !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-30] ("\\\\downarrow")
        text [30-33] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\dArr")
        text [25-28] (" !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-30] ("\\\\Downarrow")
        text [30-33] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\harr")
        text [25-28] (" !!")
      ------------------
      root [0-38]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\le")
        text [23-38] ("ftrightarrow !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\hArr")
        text [25-28] (" !!")
      ------------------
      root [0-38]
        text [0-20] ("This is an entity - ")
        entity [20-35] ("\\\\Leftrightarrow")
        text [35-38] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\crarr")
        text [26-29] (" !!")
      ------------------
      root [0-37]
        text [0-20] ("This is an entity - ")
        entity [20-34] ("\\\\hookleftarrow")
        text [34-37] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\arccos")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\arcsin")
        text [27-30] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\arctan")
        text [27-30] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\arg")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\cos")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\cos")
        text [24-28] ("h !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\cot")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\cot")
        text [24-28] ("h !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\csc")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\deg")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\det")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\dim")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\exp")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\gcd")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\hom")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\in")
        text [23-27] ("f !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\ker")
        text [24-27] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\lg")
        text [23-26] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\lim")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\lim")
        text [24-30] ("inf !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\lim")
        text [24-30] ("sup !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\ln")
        text [23-26] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\log")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\max")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\min")
        text [24-27] (" !!")
      ------------------
      root [0-26]
        text [0-20] ("This is an entity - ")
        entity [20-23] ("\\\\Pr")
        text [23-26] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sec")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sin")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sin")
        text [24-28] ("h !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sup")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\tan")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\tan")
        text [24-28] ("h !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-22] ("\\\\S")
        text [22-29] ("IGNS !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\bull")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\bull")
        text [25-30] ("et !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\star")
        text [25-28] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\lowast")
        text [27-30] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\ast")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\odot")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\oplus")
        text [26-29] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\otimes")
        text [27-30] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\check")
        text [26-29] (" !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\check")
        text [26-33] ("mark !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\para")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\ordf")
        text [25-28] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\ordm")
        text [25-28] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\cedil")
        text [26-29] (" !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\oline")
        text [26-29] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\uml")
        text [24-27] (" !!")
      ------------------
      root [0-28]
        text [0-20] ("This is an entity - ")
        entity [20-25] ("\\\\zwnj")
        text [25-28] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\zwj")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\lrm")
        text [24-27] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\rlm")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\smile")
        text [26-30] ("y !!")
      ------------------
      root [0-34]
        text [0-20] ("This is an entity - ")
        entity [20-31] ("\\\\blacksmile")
        text [31-34] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\sad")
        text [24-27] (" !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\frown")
        text [26-30] ("y !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-22] ("\\\\S")
        text [22-29] ("UITS !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\clubs")
        text [26-29] (" !!")
      ------------------
      root [0-32]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\clubs")
        text [26-32] ("uit !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\spades")
        text [27-30] (" !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\spades")
        text [27-33] ("uit !!")
      ------------------
      root [0-30]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\hearts")
        text [27-30] (" !!")
      ------------------
      root [0-33]
        text [0-20] ("This is an entity - ")
        entity [20-27] ("\\\\hearts")
        text [27-33] ("uit !!")
      ------------------
      root [0-29]
        text [0-20] ("This is an entity - ")
        entity [20-26] ("\\\\diams")
        text [26-29] (" !!")
      ------------------
      root [0-35]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\diamond")
        text [28-35] ("suit !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\diamond")
        text [28-31] (" !!")
      ------------------
      root [0-31]
        text [0-20] ("This is an entity - ")
        entity [20-28] ("\\\\Diamond")
        text [28-31] (" !!")
      ------------------
      root [0-27]
        text [0-20] ("This is an entity - ")
        entity [20-24] ("\\\\loz")
        text [24-27] (" !!")
      ------------------
      "
    `);
  });
});
