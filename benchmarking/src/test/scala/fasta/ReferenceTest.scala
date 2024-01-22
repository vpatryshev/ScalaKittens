package fasta

import java.io.File

/**
  * Created by vpatryshev on 1/30/17.
  */
class ReferenceTest extends Specification {
  val datadir = "data/references"
  val E_COLI = "e_coli"
  val S_ENTERICA = "s_enterica"
  val S_FLEXNERI = "s_flexneri"
  val V_CHOLERAE = "v_cholerae"

  sequential

  val AllNames = Set(E_COLI, S_ENTERICA, S_FLEXNERI, V_CHOLERAE)

  val E_COLI_FRAG1: String = "NC_000913.3 Escherichia coli str. K-12 substr. MG1655, complete genome"
  val S_ENTERICA_FRAG1: String = "NC_003198.1 Salmonella enterica subsp. enterica serovar Typhi str. CT18, complete genome"
  val S_FLEXNERI_FRAG1: String = "NC_004337.2 Shigella flexneri 2a str. 301 chromosome, complete genome"
  val V_CHOLERA_FRAG1: String = "NC_002505.1 Vibrio cholerae O1 biovar El Tor str. N16961 chromosome I, complete sequence"

  "ListRefFiles" should {
    "list" in {
      val files = Reference.listRefFiles(datadir)
      files.size must_== 4

      for (name <- AllNames) {
        files.exists(_.getName startsWith name) must beTrue
      }
      ok
    }
  }

  "Name extractor" should {
    "behave" in {
      Reference.nameFrom(new File(s"$datadir/$E_COLI.fasta")) must_== E_COLI
      Reference.nameFrom(new File(s"/etc/$E_COLI.fasta")) must_== E_COLI
      try {
        Reference.nameFrom(new File(s"$datadir/$E_COLI.fast"))
      } catch {
        case oops: Bad => // good
      }
      ok
    }
  }

  "Reference" should {

    "load e_coli" in {
      val filename = s"$datadir/$E_COLI.fasta"
      val sample: String = "ATGTCGATCGCCATTATGGCCGGCGTATTAGAAGCGCGCG"
        
      val ref = Reference(filename)
      ref.name must_== E_COLI

      val alignments: List[(String, MatchPoint)] = ref.alignments(sample, 10, 11)
      alignments must_== List(
        (E_COLI_FRAG1, MatchPoint(0, 720)),
        (E_COLI_FRAG1,MatchPoint(0,282488)),
        (E_COLI_FRAG1, MatchPoint(1,1985905)),
        (E_COLI_FRAG1,MatchPoint(0,2172295)),
        (E_COLI_FRAG1,MatchPoint(4,2359155)),
        (E_COLI_FRAG1,MatchPoint(0,2433150)),
        (E_COLI_FRAG1,MatchPoint(0,2800816)),
        (E_COLI_FRAG1,MatchPoint(1,2889798)),
        (E_COLI_FRAG1,MatchPoint(0,3022215)),
        (E_COLI_FRAG1,MatchPoint(1,3421649)),
        (E_COLI_FRAG1,MatchPoint(1,4530077))
      )

    }
    
    "load s_enterica" in {
      val filename = s"$datadir/$S_ENTERICA.fasta"

      val ref = Reference(filename)
      ref.name must_== S_ENTERICA
      val alignments: List[(String, MatchPoint)] = ref.alignments("GATGACACTACGACACAACAGACGATGCGCGAGCTGAAAGCGAAAGGTTATA")
      alignments must_== (S_ENTERICA_FRAG1, MatchPoint(0,4726))::Nil
    }

    "load s_flexneri" in {
      val filename = s"$datadir/$S_FLEXNERI.fasta"

      val ref = Reference(filename)
      ref.name must_== S_FLEXNERI
      ref.alignments("GGCATTACCTCGAATCTACCGTCGATATTGCTGAGTCCACCCGCCGTATTGCGG") must_== (S_FLEXNERI_FRAG1, MatchPoint(0,807))::Nil
    }

    "load v_cholerae" in {
      val filename = s"$datadir/$V_CHOLERAE.fasta"

      val ref = Reference(filename)
      ref.name must_== V_CHOLERAE
      ref.alignments("AATCGCTAAGTGCTCTGCGGCTCGCTCTAGGGCATCGAGATGACGGCGGCG") must_== (V_CHOLERA_FRAG1, MatchPoint(0,965))::Nil
    }
  }

  "All References" should {
    "load and use" in {
      val refs = Reference.readAll(datadir)
      refs.size must_== 4
      refs(E_COLI).alignments("ATGTCGATCGCCATTATGGCCGGCGTATTAGAAGCGCGCG") must_== (E_COLI_FRAG1, MatchPoint(0,720)) :: Nil

      refs(S_ENTERICA).alignments("GATGACACTACGACACAACAGACGATGCGCGAGCTGAAAGCGAAAGGTTATA") must_== (S_ENTERICA_FRAG1, MatchPoint(0,4726)) :: Nil

      refs(S_FLEXNERI).alignments("GGCATTACCTCGAATCTACCGTCGATATTGCTGAGTCCACCCGCCGTATTGCGG") must_== (S_FLEXNERI_FRAG1, MatchPoint(0,807)) :: Nil

      refs(V_CHOLERAE) alignments "AATCGCTAAGTGCTCTGCGGCTCGCTCTAGGGCATCGAGATGACGGCGGCG" must_== (V_CHOLERA_FRAG1, MatchPoint(0,965)) :: Nil
    }
  }
}

