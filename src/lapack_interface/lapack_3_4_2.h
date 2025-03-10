#ifndef LAPACK_H
#define LAPACK_H

#include <stdint.h>

#include "flexiblas_config.h"

#include "flexiblas_fortran_mangle.h"

#include "flexiblas_fortran_char_len.h"

/* Complex type (single precision) */
#ifndef lapack_complex_float
#ifndef __cplusplus
#include <complex.h>
#else
#include <complex>
#endif
#define lapack_complex_float    float _Complex
#endif

/* Complex type (double precision) */
#ifndef lapack_complex_double
#ifndef __cplusplus
#include <complex.h>
#else
#include <complex>
#endif
#define lapack_complex_double   double _Complex
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined(USE_BLAS_32) && defined(USE_BLAS_64)
#error Either USE_BLAS_32 or USE_BLAS_64 must be defined!
#endif

#ifndef blasint
#define blasint int
#define CBBCSD   cbbcsd_
#define CBDSQR   cbdsqr_
#define CGBBRD   cgbbrd_
#define CGBCON   cgbcon_
#define CGBEQU   cgbequ_
#define CGBEQUB  cgbequb_
#define CGBRFS   cgbrfs_
#define CGBSV    cgbsv_
#define CGBSVX   cgbsvx_
#define CGBTF2   cgbtf2_
#define CGBTRF   cgbtrf_
#define CGBTRS   cgbtrs_
#define CGEBAK   cgebak_
#define CGEBAL   cgebal_
#define CGEBD2   cgebd2_
#define CGEBRD   cgebrd_
#define CGECON   cgecon_
#define CGEEQU   cgeequ_
#define CGEEQUB  cgeequb_
#define CGEES    cgees_
#define CGEESX   cgeesx_
#define CGEEV    cgeev_
#define CGEEVX   cgeevx_
#define CGEGS    cgegs_
#define CGEGV    cgegv_
#define CGEHD2   cgehd2_
#define CGEHRD   cgehrd_
#define CGELQ2   cgelq2_
#define CGELQF   cgelqf_
#define CGELS    cgels_
#define CGELSD   cgelsd_
#define CGELSS   cgelss_
#define CGELSX   cgelsx_
#define CGELSY   cgelsy_
#define CGEMQRT  cgemqrt_
#define CGEQL2   cgeql2_
#define CGEQLF   cgeqlf_
#define CGEQP3   cgeqp3_
#define CGEQPF   cgeqpf_
#define CGEQR2   cgeqr2_
#define CGEQR2P  cgeqr2p_
#define CGEQRF   cgeqrf_
#define CGEQRFP  cgeqrfp_
#define CGEQRT   cgeqrt_
#define CGEQRT2  cgeqrt2_
#define CGEQRT3  cgeqrt3_
#define CGERFS   cgerfs_
#define CGERQ2   cgerq2_
#define CGERQF   cgerqf_
#define CGESC2   cgesc2_
#define CGESDD   cgesdd_
#define CGESV    cgesv_
#define CGESVD   cgesvd_
#define CGESVX   cgesvx_
#define CGETC2   cgetc2_
#define CGETF2   cgetf2_
#define CGETRF   cgetrf_
#define CGETRI   cgetri_
#define CGETRS   cgetrs_
#define CGGBAK   cggbak_
#define CGGBAL   cggbal_
#define CGGES    cgges_
#define CGGESX   cggesx_
#define CGGEV    cggev_
#define CGGEVX   cggevx_
#define CGGGLM   cggglm_
#define CGGHRD   cgghrd_
#define CGGLSE   cgglse_
#define CGGQRF   cggqrf_
#define CGGRQF   cggrqf_
#define CGGSVD   cggsvd_
#define CGGSVP   cggsvp_
#define CGTCON   cgtcon_
#define CGTRFS   cgtrfs_
#define CGTSV    cgtsv_
#define CGTSVX   cgtsvx_
#define CGTTRF   cgttrf_
#define CGTTRS   cgttrs_
#define CGTTS2   cgtts2_
#define CHBEV    chbev_
#define CHBEVD   chbevd_
#define CHBEVX   chbevx_
#define CHBGST   chbgst_
#define CHBGV    chbgv_
#define CHBGVD   chbgvd_
#define CHBGVX   chbgvx_
#define CHBTRD   chbtrd_
#define CHECON   checon_
#define CHEEQUB  cheequb_
#define CHEEV    cheev_
#define CHEEVD   cheevd_
#define CHEEVR   cheevr_
#define CHEEVX   cheevx_
#define CHEGS2   chegs2_
#define CHEGST   chegst_
#define CHEGV    chegv_
#define CHEGVD   chegvd_
#define CHEGVX   chegvx_
#define CHERFS   cherfs_
#define CHESV    chesv_
#define CHESVX   chesvx_
#define CHESWAPR cheswapr_
#define CHETD2   chetd2_
#define CHETF2   chetf2_
#define CHETRD   chetrd_
#define CHETRF   chetrf_
#define CHETRI   chetri_
#define CHETRI2  chetri2_
#define CHETRI2X chetri2x_
#define CHETRS   chetrs_
#define CHETRS2  chetrs2_
#define CHFRK    chfrk_
#define CHGEQZ   chgeqz_
#define CHLA_TRANSTYPE chla_transtype_
#define CHPCON   chpcon_
#define CHPEV    chpev_
#define CHPEVD   chpevd_
#define CHPEVX   chpevx_
#define CHPGST   chpgst_
#define CHPGV    chpgv_
#define CHPGVD   chpgvd_
#define CHPGVX   chpgvx_
#define CHPRFS   chprfs_
#define CHPSV    chpsv_
#define CHPSVX   chpsvx_
#define CHPTRD   chptrd_
#define CHPTRF   chptrf_
#define CHPTRI   chptri_
#define CHPTRS   chptrs_
#define CHSEIN   chsein_
#define CHSEQR   chseqr_
#define CLABRD   clabrd_
#define CLACGV   clacgv_
#define CLACN2   clacn2_
#define CLACON   clacon_
#define CLACP2   clacp2_
#define CLACPY   clacpy_
#define CLACRM   clacrm_
#define CLACRT   clacrt_
#define CLADIV   cladiv_
#define CLAED0   claed0_
#define CLAED7   claed7_
#define CLAED8   claed8_
#define CLAEIN   claein_
#define CLAESY   claesy_
#define CLAEV2   claev2_
#define CLAG2Z   clag2z_
#define CLAGS2   clags2_
#define CLAGTM   clagtm_
#define CLAHEF   clahef_
#define CLAHQR   clahqr_
#define CLAHR2   clahr2_
#define CLAHRD   clahrd_
#define CLAIC1   claic1_
#define CLALS0   clals0_
#define CLALSA   clalsa_
#define CLALSD   clalsd_
#define CLANGB   clangb_
#define CLANGE   clange_
#define CLANGT   clangt_
#define CLANHB   clanhb_
#define CLANHE   clanhe_
#define CLANHF   clanhf_
#define CLANHP   clanhp_
#define CLANHS   clanhs_
#define CLANHT   clanht_
#define CLANSB   clansb_
#define CLANSP   clansp_
#define CLANSY   clansy_
#define CLANTB   clantb_
#define CLANTP   clantp_
#define CLANTR   clantr_
#define CLAPLL   clapll_
#define CLAPMR   clapmr_
#define CLAPMT   clapmt_
#define CLAQGB   claqgb_
#define CLAQGE   claqge_
#define CLAQHB   claqhb_
#define CLAQHE   claqhe_
#define CLAQHP   claqhp_
#define CLAQP2   claqp2_
#define CLAQPS   claqps_
#define CLAQR0   claqr0_
#define CLAQR1   claqr1_
#define CLAQR2   claqr2_
#define CLAQR3   claqr3_
#define CLAQR4   claqr4_
#define CLAQR5   claqr5_
#define CLAQSB   claqsb_
#define CLAQSP   claqsp_
#define CLAQSY   claqsy_
#define CLAR1V   clar1v_
#define CLAR2V   clar2v_
#define CLARCM   clarcm_
#define CLARF    clarf_
#define CLARFB   clarfb_
#define CLARFG   clarfg_
#define CLARFGP  clarfgp_
#define CLARFT   clarft_
#define CLARFX   clarfx_
#define CLARGV   clargv_
#define CLARNV   clarnv_
#define CLARRV   clarrv_
#define CLARTG   clartg_
#define CLARTV   clartv_
#define CLARZ    clarz_
#define CLARZB   clarzb_
#define CLARZT   clarzt_
#define CLASCL   clascl_
#define CLASET   claset_
#define CLASR    clasr_
#define CLASSQ   classq_
#define CLASWP   claswp_
#define CLASYF   clasyf_
#define CLATBS   clatbs_
#define CLATDF   clatdf_
#define CLATPS   clatps_
#define CLATRD   clatrd_
#define CLATRS   clatrs_
#define CLATRZ   clatrz_
#define CLATZM   clatzm_
#define CLAUU2   clauu2_
#define CLAUUM   clauum_
#define CPBCON   cpbcon_
#define CPBEQU   cpbequ_
#define CPBRFS   cpbrfs_
#define CPBSTF   cpbstf_
#define CPBSV    cpbsv_
#define CPBSVX   cpbsvx_
#define CPBTF2   cpbtf2_
#define CPBTRF   cpbtrf_
#define CPBTRS   cpbtrs_
#define CPFTRF   cpftrf_
#define CPFTRI   cpftri_
#define CPFTRS   cpftrs_
#define CPOCON   cpocon_
#define CPOEQU   cpoequ_
#define CPOEQUB  cpoequb_
#define CPORFS   cporfs_
#define CPOSV    cposv_
#define CPOSVX   cposvx_
#define CPOTF2   cpotf2_
#define CPOTRF   cpotrf_
#define CPOTRI   cpotri_
#define CPOTRS   cpotrs_
#define CPPCON   cppcon_
#define CPPEQU   cppequ_
#define CPPRFS   cpprfs_
#define CPPSV    cppsv_
#define CPPSVX   cppsvx_
#define CPPTRF   cpptrf_
#define CPPTRI   cpptri_
#define CPPTRS   cpptrs_
#define CPSTF2   cpstf2_
#define CPSTRF   cpstrf_
#define CPTCON   cptcon_
#define CPTEQR   cpteqr_
#define CPTRFS   cptrfs_
#define CPTSV    cptsv_
#define CPTSVX   cptsvx_
#define CPTTRF   cpttrf_
#define CPTTRS   cpttrs_
#define CPTTS2   cptts2_
#define CROT     crot_
#define CSPCON   cspcon_
#define CSPMV    cspmv_
#define CSPR     cspr_
#define CSPRFS   csprfs_
#define CSPSV    cspsv_
#define CSPSVX   cspsvx_
#define CSPTRF   csptrf_
#define CSPTRI   csptri_
#define CSPTRS   csptrs_
#define CSRSCL   csrscl_
#define CSTEDC   cstedc_
#define CSTEGR   cstegr_
#define CSTEIN   cstein_
#define CSTEMR   cstemr_
#define CSTEQR   csteqr_
#define CSYCON   csycon_
#define CSYCONV  csyconv_
#define CSYEQUB  csyequb_
#define CSYMV    csymv_
#define CSYR     csyr_
#define CSYRFS   csyrfs_
#define CSYSV    csysv_
#define CSYSVX   csysvx_
#define CSYSWAPR csyswapr_
#define CSYTF2   csytf2_
#define CSYTRF   csytrf_
#define CSYTRI   csytri_
#define CSYTRI2  csytri2_
#define CSYTRI2X csytri2x_
#define CSYTRS   csytrs_
#define CSYTRS2  csytrs2_
#define CTBCON   ctbcon_
#define CTBRFS   ctbrfs_
#define CTBTRS   ctbtrs_
#define CTFSM    ctfsm_
#define CTFTRI   ctftri_
#define CTFTTP   ctfttp_
#define CTFTTR   ctfttr_
#define CTGEVC   ctgevc_
#define CTGEX2   ctgex2_
#define CTGEXC   ctgexc_
#define CTGSEN   ctgsen_
#define CTGSJA   ctgsja_
#define CTGSNA   ctgsna_
#define CTGSY2   ctgsy2_
#define CTGSYL   ctgsyl_
#define CTPCON   ctpcon_
#define CTPMQRT  ctpmqrt_
#define CTPQRT   ctpqrt_
#define CTPQRT2  ctpqrt2_
#define CTPRFB   ctprfb_
#define CTPRFS   ctprfs_
#define CTPTRI   ctptri_
#define CTPTRS   ctptrs_
#define CTPTTF   ctpttf_
#define CTPTTR   ctpttr_
#define CTRCON   ctrcon_
#define CTREVC   ctrevc_
#define CTREXC   ctrexc_
#define CTRRFS   ctrrfs_
#define CTRSEN   ctrsen_
#define CTRSNA   ctrsna_
#define CTRSYL   ctrsyl_
#define CTRTI2   ctrti2_
#define CTRTRI   ctrtri_
#define CTRTRS   ctrtrs_
#define CTRTTF   ctrttf_
#define CTRTTP   ctrttp_
#define CTZRQF   ctzrqf_
#define CTZRZF   ctzrzf_
#define CUNBDB   cunbdb_
#define CUNCSD   cuncsd_
#define CUNG2L   cung2l_
#define CUNG2R   cung2r_
#define CUNGBR   cungbr_
#define CUNGHR   cunghr_
#define CUNGL2   cungl2_
#define CUNGLQ   cunglq_
#define CUNGQL   cungql_
#define CUNGQR   cungqr_
#define CUNGR2   cungr2_
#define CUNGRQ   cungrq_
#define CUNGTR   cungtr_
#define CUNM2L   cunm2l_
#define CUNM2R   cunm2r_
#define CUNMBR   cunmbr_
#define CUNMHR   cunmhr_
#define CUNML2   cunml2_
#define CUNMLQ   cunmlq_
#define CUNMQL   cunmql_
#define CUNMQR   cunmqr_
#define CUNMR2   cunmr2_
#define CUNMR3   cunmr3_
#define CUNMRQ   cunmrq_
#define CUNMRZ   cunmrz_
#define CUNMTR   cunmtr_
#define CUPGTR   cupgtr_
#define CUPMTR   cupmtr_
#define DBBCSD   dbbcsd_
#define DBDSDC   dbdsdc_
#define DBDSQR   dbdsqr_
#define DDISNA   ddisna_
#define DGBBRD   dgbbrd_
#define DGBCON   dgbcon_
#define DGBEQU   dgbequ_
#define DGBEQUB  dgbequb_
#define DGBRFS   dgbrfs_
#define DGBSV    dgbsv_
#define DGBSVX   dgbsvx_
#define DGBTF2   dgbtf2_
#define DGBTRF   dgbtrf_
#define DGBTRS   dgbtrs_
#define DGEBAK   dgebak_
#define DGEBAL   dgebal_
#define DGEBD2   dgebd2_
#define DGEBRD   dgebrd_
#define DGECON   dgecon_
#define DGEEQU   dgeequ_
#define DGEEQUB  dgeequb_
#define DGEES    dgees_
#define DGEESX   dgeesx_
#define DGEEV    dgeev_
#define DGEEVX   dgeevx_
#define DGEGS    dgegs_
#define DGEGV    dgegv_
#define DGEHD2   dgehd2_
#define DGEHRD   dgehrd_
#define DGEJSV   dgejsv_
#define DGELQ2   dgelq2_
#define DGELQF   dgelqf_
#define DGELS    dgels_
#define DGELSD   dgelsd_
#define DGELSS   dgelss_
#define DGELSX   dgelsx_
#define DGELSY   dgelsy_
#define DGEMQRT  dgemqrt_
#define DGEQL2   dgeql2_
#define DGEQLF   dgeqlf_
#define DGEQP3   dgeqp3_
#define DGEQPF   dgeqpf_
#define DGEQR2   dgeqr2_
#define DGEQR2P  dgeqr2p_
#define DGEQRF   dgeqrf_
#define DGEQRFP  dgeqrfp_
#define DGEQRT   dgeqrt_
#define DGEQRT2  dgeqrt2_
#define DGEQRT3  dgeqrt3_
#define DGERFS   dgerfs_
#define DGERQ2   dgerq2_
#define DGERQF   dgerqf_
#define DGESC2   dgesc2_
#define DGESDD   dgesdd_
#define DGESV    dgesv_
#define DGESVD   dgesvd_
#define DGESVJ   dgesvj_
#define DGESVX   dgesvx_
#define DGETC2   dgetc2_
#define DGETF2   dgetf2_
#define DGETRF   dgetrf_
#define DGETRI   dgetri_
#define DGETRS   dgetrs_
#define DGGBAK   dggbak_
#define DGGBAL   dggbal_
#define DGGES    dgges_
#define DGGESX   dggesx_
#define DGGEV    dggev_
#define DGGEVX   dggevx_
#define DGGGLM   dggglm_
#define DGGHRD   dgghrd_
#define DGGLSE   dgglse_
#define DGGQRF   dggqrf_
#define DGGRQF   dggrqf_
#define DGGSVD   dggsvd_
#define DGGSVP   dggsvp_
#define DGSVJ0   dgsvj0_
#define DGSVJ1   dgsvj1_
#define DGTCON   dgtcon_
#define DGTRFS   dgtrfs_
#define DGTSV    dgtsv_
#define DGTSVX   dgtsvx_
#define DGTTRF   dgttrf_
#define DGTTRS   dgttrs_
#define DGTTS2   dgtts2_
#define DHGEQZ   dhgeqz_
#define DHSEIN   dhsein_
#define DHSEQR   dhseqr_
#define DISNAN   disnan_
#define DLABAD   dlabad_
#define DLABRD   dlabrd_
#define DLACN2   dlacn2_
#define DLACON   dlacon_
#define DLACPY   dlacpy_
#define DLADIV   dladiv_
#define DLAE2    dlae2_
#define DLAEBZ   dlaebz_
#define DLAED0   dlaed0_
#define DLAED1   dlaed1_
#define DLAED2   dlaed2_
#define DLAED3   dlaed3_
#define DLAED4   dlaed4_
#define DLAED5   dlaed5_
#define DLAED6   dlaed6_
#define DLAED7   dlaed7_
#define DLAED8   dlaed8_
#define DLAED9   dlaed9_
#define DLAEDA   dlaeda_
#define DLAEIN   dlaein_
#define DLAEV2   dlaev2_
#define DLAEXC   dlaexc_
#define DLAG2    dlag2_
#define DLAG2S   dlag2s_
#define DLAGS2   dlags2_
#define DLAGTF   dlagtf_
#define DLAGTM   dlagtm_
#define DLAGTS   dlagts_
#define DLAGV2   dlagv2_
#define DLAHQR   dlahqr_
#define DLAHR2   dlahr2_
#define DLAHRD   dlahrd_
#define DLAIC1   dlaic1_
#define DLAISNAN dlaisnan_
#define DLALN2   dlaln2_
#define DLALS0   dlals0_
#define DLALSA   dlalsa_
#define DLALSD   dlalsd_
#define DLAMC3   dlamc3_
#define DLAMCH   dlamch_
#define DLAMRG   dlamrg_
#define DLANEG   dlaneg_
#define DLANGB   dlangb_
#define DLANGE   dlange_
#define DLANGT   dlangt_
#define DLANHS   dlanhs_
#define DLANSB   dlansb_
#define DLANSF   dlansf_
#define DLANSP   dlansp_
#define DLANST   dlanst_
#define DLANSY   dlansy_
#define DLANTB   dlantb_
#define DLANTP   dlantp_
#define DLANTR   dlantr_
#define DLANV2   dlanv2_
#define DLAPLL   dlapll_
#define DLAPMR   dlapmr_
#define DLAPMT   dlapmt_
#define DLAPY2   dlapy2_
#define DLAPY3   dlapy3_
#define DLAQGB   dlaqgb_
#define DLAQGE   dlaqge_
#define DLAQP2   dlaqp2_
#define DLAQPS   dlaqps_
#define DLAQR0   dlaqr0_
#define DLAQR1   dlaqr1_
#define DLAQR2   dlaqr2_
#define DLAQR3   dlaqr3_
#define DLAQR4   dlaqr4_
#define DLAQR5   dlaqr5_
#define DLAQSB   dlaqsb_
#define DLAQSP   dlaqsp_
#define DLAQSY   dlaqsy_
#define DLAQTR   dlaqtr_
#define DLAR1V   dlar1v_
#define DLAR2V   dlar2v_
#define DLARF    dlarf_
#define DLARFB   dlarfb_
#define DLARFG   dlarfg_
#define DLARFGP  dlarfgp_
#define DLARFT   dlarft_
#define DLARFX   dlarfx_
#define DLARGV   dlargv_
#define DLARNV   dlarnv_
#define DLARRA   dlarra_
#define DLARRB   dlarrb_
#define DLARRC   dlarrc_
#define DLARRD   dlarrd_
#define DLARRE   dlarre_
#define DLARRF   dlarrf_
#define DLARRJ   dlarrj_
#define DLARRK   dlarrk_
#define DLARRR   dlarrr_
#define DLARRV   dlarrv_
#define DLARTG   dlartg_
#define DLARTGP  dlartgp_
#define DLARTGS  dlartgs_
#define DLARTV   dlartv_
#define DLARUV   dlaruv_
#define DLARZ    dlarz_
#define DLARZB   dlarzb_
#define DLARZT   dlarzt_
#define DLAS2    dlas2_
#define DLASCL   dlascl_
#define DLASD0   dlasd0_
#define DLASD1   dlasd1_
#define DLASD2   dlasd2_
#define DLASD3   dlasd3_
#define DLASD4   dlasd4_
#define DLASD5   dlasd5_
#define DLASD6   dlasd6_
#define DLASD7   dlasd7_
#define DLASD8   dlasd8_
#define DLASDA   dlasda_
#define DLASDQ   dlasdq_
#define DLASDT   dlasdt_
#define DLASET   dlaset_
#define DLASQ1   dlasq1_
#define DLASQ2   dlasq2_
#define DLASQ3   dlasq3_
#define DLASQ4   dlasq4_
#define DLASQ5   dlasq5_
#define DLASQ6   dlasq6_
#define DLASR    dlasr_
#define DLASRT   dlasrt_
#define DLASSQ   dlassq_
#define DLASV2   dlasv2_
#define DLASWP   dlaswp_
#define DLASY2   dlasy2_
#define DLASYF   dlasyf_
#define DLAT2S   dlat2s_
#define DLATBS   dlatbs_
#define DLATDF   dlatdf_
#define DLATPS   dlatps_
#define DLATRD   dlatrd_
#define DLATRS   dlatrs_
#define DLATRZ   dlatrz_
#define DLATZM   dlatzm_
#define DLAUU2   dlauu2_
#define DLAUUM   dlauum_
#define DOPGTR   dopgtr_
#define DOPMTR   dopmtr_
#define DORBDB   dorbdb_
#define DORCSD   dorcsd_
#define DORG2L   dorg2l_
#define DORG2R   dorg2r_
#define DORGBR   dorgbr_
#define DORGHR   dorghr_
#define DORGL2   dorgl2_
#define DORGLQ   dorglq_
#define DORGQL   dorgql_
#define DORGQR   dorgqr_
#define DORGR2   dorgr2_
#define DORGRQ   dorgrq_
#define DORGTR   dorgtr_
#define DORM2L   dorm2l_
#define DORM2R   dorm2r_
#define DORMBR   dormbr_
#define DORMHR   dormhr_
#define DORML2   dorml2_
#define DORMLQ   dormlq_
#define DORMQL   dormql_
#define DORMQR   dormqr_
#define DORMR2   dormr2_
#define DORMR3   dormr3_
#define DORMRQ   dormrq_
#define DORMRZ   dormrz_
#define DORMTR   dormtr_
#define DPBCON   dpbcon_
#define DPBEQU   dpbequ_
#define DPBRFS   dpbrfs_
#define DPBSTF   dpbstf_
#define DPBSV    dpbsv_
#define DPBSVX   dpbsvx_
#define DPBTF2   dpbtf2_
#define DPBTRF   dpbtrf_
#define DPBTRS   dpbtrs_
#define DPFTRF   dpftrf_
#define DPFTRI   dpftri_
#define DPFTRS   dpftrs_
#define DPOCON   dpocon_
#define DPOEQU   dpoequ_
#define DPOEQUB  dpoequb_
#define DPORFS   dporfs_
#define DPOSV    dposv_
#define DPOSVX   dposvx_
#define DPOTF2   dpotf2_
#define DPOTRF   dpotrf_
#define DPOTRI   dpotri_
#define DPOTRS   dpotrs_
#define DPPCON   dppcon_
#define DPPEQU   dppequ_
#define DPPRFS   dpprfs_
#define DPPSV    dppsv_
#define DPPSVX   dppsvx_
#define DPPTRF   dpptrf_
#define DPPTRI   dpptri_
#define DPPTRS   dpptrs_
#define DPSTF2   dpstf2_
#define DPSTRF   dpstrf_
#define DPTCON   dptcon_
#define DPTEQR   dpteqr_
#define DPTRFS   dptrfs_
#define DPTSV    dptsv_
#define DPTSVX   dptsvx_
#define DPTTRF   dpttrf_
#define DPTTRS   dpttrs_
#define DPTTS2   dptts2_
#define DRSCL    drscl_
#define DSBEV    dsbev_
#define DSBEVD   dsbevd_
#define DSBEVX   dsbevx_
#define DSBGST   dsbgst_
#define DSBGV    dsbgv_
#define DSBGVD   dsbgvd_
#define DSBGVX   dsbgvx_
#define DSBTRD   dsbtrd_
#define DSECND   dsecnd_
#define DSFRK    dsfrk_
#define DSGESV   dsgesv_
#define DSPCON   dspcon_
#define DSPEV    dspev_
#define DSPEVD   dspevd_
#define DSPEVX   dspevx_
#define DSPGST   dspgst_
#define DSPGV    dspgv_
#define DSPGVD   dspgvd_
#define DSPGVX   dspgvx_
#define DSPOSV   dsposv_
#define DSPRFS   dsprfs_
#define DSPSV    dspsv_
#define DSPSVX   dspsvx_
#define DSPTRD   dsptrd_
#define DSPTRF   dsptrf_
#define DSPTRI   dsptri_
#define DSPTRS   dsptrs_
#define DSTEBZ   dstebz_
#define DSTEDC   dstedc_
#define DSTEGR   dstegr_
#define DSTEIN   dstein_
#define DSTEMR   dstemr_
#define DSTEQR   dsteqr_
#define DSTERF   dsterf_
#define DSTEV    dstev_
#define DSTEVD   dstevd_
#define DSTEVR   dstevr_
#define DSTEVX   dstevx_
#define DSYCON   dsycon_
#define DSYCONV  dsyconv_
#define DSYEQUB  dsyequb_
#define DSYEV    dsyev_
#define DSYEVD   dsyevd_
#define DSYEVR   dsyevr_
#define DSYEVX   dsyevx_
#define DSYGS2   dsygs2_
#define DSYGST   dsygst_
#define DSYGV    dsygv_
#define DSYGVD   dsygvd_
#define DSYGVX   dsygvx_
#define DSYRFS   dsyrfs_
#define DSYSV    dsysv_
#define DSYSVX   dsysvx_
#define DSYSWAPR dsyswapr_
#define DSYTD2   dsytd2_
#define DSYTF2   dsytf2_
#define DSYTRD   dsytrd_
#define DSYTRF   dsytrf_
#define DSYTRI   dsytri_
#define DSYTRI2  dsytri2_
#define DSYTRI2X dsytri2x_
#define DSYTRS   dsytrs_
#define DSYTRS2  dsytrs2_
#define DTBCON   dtbcon_
#define DTBRFS   dtbrfs_
#define DTBTRS   dtbtrs_
#define DTFSM    dtfsm_
#define DTFTRI   dtftri_
#define DTFTTP   dtfttp_
#define DTFTTR   dtfttr_
#define DTGEVC   dtgevc_
#define DTGEX2   dtgex2_
#define DTGEXC   dtgexc_
#define DTGSEN   dtgsen_
#define DTGSJA   dtgsja_
#define DTGSNA   dtgsna_
#define DTGSY2   dtgsy2_
#define DTGSYL   dtgsyl_
#define DTPCON   dtpcon_
#define DTPMQRT  dtpmqrt_
#define DTPQRT   dtpqrt_
#define DTPQRT2  dtpqrt2_
#define DTPRFB   dtprfb_
#define DTPRFS   dtprfs_
#define DTPTRI   dtptri_
#define DTPTRS   dtptrs_
#define DTPTTF   dtpttf_
#define DTPTTR   dtpttr_
#define DTRCON   dtrcon_
#define DTREVC   dtrevc_
#define DTREXC   dtrexc_
#define DTRRFS   dtrrfs_
#define DTRSEN   dtrsen_
#define DTRSNA   dtrsna_
#define DTRSYL   dtrsyl_
#define DTRTI2   dtrti2_
#define DTRTRI   dtrtri_
#define DTRTRS   dtrtrs_
#define DTRTTF   dtrttf_
#define DTRTTP   dtrttp_
#define DTZRQF   dtzrqf_
#define DTZRZF   dtzrzf_
#define DZSUM1   dzsum1_
#define ICMAX1   icmax1_
#define IEEECK   ieeeck_
#define ILACLC   ilaclc_
#define ILACLR   ilaclr_
#define ILADIAG  iladiag_
#define ILADLC   iladlc_
#define ILADLR   iladlr_
#define ILAENV   ilaenv_
#define ILAPREC  ilaprec_
#define ILASLC   ilaslc_
#define ILASLR   ilaslr_
#define ILATRANS ilatrans_
#define ILAUPLO  ilauplo_
#define ILAZLC   ilazlc_
#define ILAZLR   ilazlr_
#define IPARMQ   iparmq_
#define IZMAX1   izmax1_
#define SBBCSD   sbbcsd_
#define SBDSDC   sbdsdc_
#define SBDSQR   sbdsqr_
#define SCSUM1   scsum1_
#define SDISNA   sdisna_
#define SECOND   second_
#define SGBBRD   sgbbrd_
#define SGBCON   sgbcon_
#define SGBEQU   sgbequ_
#define SGBEQUB  sgbequb_
#define SGBRFS   sgbrfs_
#define SGBSV    sgbsv_
#define SGBSVX   sgbsvx_
#define SGBTF2   sgbtf2_
#define SGBTRF   sgbtrf_
#define SGBTRS   sgbtrs_
#define SGEBAK   sgebak_
#define SGEBAL   sgebal_
#define SGEBD2   sgebd2_
#define SGEBRD   sgebrd_
#define SGECON   sgecon_
#define SGEEQU   sgeequ_
#define SGEEQUB  sgeequb_
#define SGEES    sgees_
#define SGEESX   sgeesx_
#define SGEEV    sgeev_
#define SGEEVX   sgeevx_
#define SGEGS    sgegs_
#define SGEGV    sgegv_
#define SGEHD2   sgehd2_
#define SGEHRD   sgehrd_
#define SGEJSV   sgejsv_
#define SGELQ2   sgelq2_
#define SGELQF   sgelqf_
#define SGELS    sgels_
#define SGELSD   sgelsd_
#define SGELSS   sgelss_
#define SGELSX   sgelsx_
#define SGELSY   sgelsy_
#define SGEMQRT  sgemqrt_
#define SGEQL2   sgeql2_
#define SGEQLF   sgeqlf_
#define SGEQP3   sgeqp3_
#define SGEQPF   sgeqpf_
#define SGEQR2   sgeqr2_
#define SGEQR2P  sgeqr2p_
#define SGEQRF   sgeqrf_
#define SGEQRFP  sgeqrfp_
#define SGEQRT   sgeqrt_
#define SGEQRT2  sgeqrt2_
#define SGEQRT3  sgeqrt3_
#define SGERFS   sgerfs_
#define SGERQ2   sgerq2_
#define SGERQF   sgerqf_
#define SGESC2   sgesc2_
#define SGESDD   sgesdd_
#define SGESV    sgesv_
#define SGESVD   sgesvd_
#define SGESVJ   sgesvj_
#define SGESVX   sgesvx_
#define SGETC2   sgetc2_
#define SGETF2   sgetf2_
#define SGETRF   sgetrf_
#define SGETRI   sgetri_
#define SGETRS   sgetrs_
#define SGGBAK   sggbak_
#define SGGBAL   sggbal_
#define SGGES    sgges_
#define SGGESX   sggesx_
#define SGGEV    sggev_
#define SGGEVX   sggevx_
#define SGGGLM   sggglm_
#define SGGHRD   sgghrd_
#define SGGLSE   sgglse_
#define SGGQRF   sggqrf_
#define SGGRQF   sggrqf_
#define SGGSVD   sggsvd_
#define SGGSVP   sggsvp_
#define SGSVJ0   sgsvj0_
#define SGSVJ1   sgsvj1_
#define SGTCON   sgtcon_
#define SGTRFS   sgtrfs_
#define SGTSV    sgtsv_
#define SGTSVX   sgtsvx_
#define SGTTRF   sgttrf_
#define SGTTRS   sgttrs_
#define SGTTS2   sgtts2_
#define SHGEQZ   shgeqz_
#define SHSEIN   shsein_
#define SHSEQR   shseqr_
#define SISNAN   sisnan_
#define SLABAD   slabad_
#define SLABRD   slabrd_
#define SLACN2   slacn2_
#define SLACON   slacon_
#define SLACPY   slacpy_
#define SLADIV   sladiv_
#define SLAE2    slae2_
#define SLAEBZ   slaebz_
#define SLAED0   slaed0_
#define SLAED1   slaed1_
#define SLAED2   slaed2_
#define SLAED3   slaed3_
#define SLAED4   slaed4_
#define SLAED5   slaed5_
#define SLAED6   slaed6_
#define SLAED7   slaed7_
#define SLAED8   slaed8_
#define SLAED9   slaed9_
#define SLAEDA   slaeda_
#define SLAEIN   slaein_
#define SLAEV2   slaev2_
#define SLAEXC   slaexc_
#define SLAG2    slag2_
#define SLAG2D   slag2d_
#define SLAGS2   slags2_
#define SLAGTF   slagtf_
#define SLAGTM   slagtm_
#define SLAGTS   slagts_
#define SLAGV2   slagv2_
#define SLAHQR   slahqr_
#define SLAHR2   slahr2_
#define SLAHRD   slahrd_
#define SLAIC1   slaic1_
#define SLAISNAN slaisnan_
#define SLALN2   slaln2_
#define SLALS0   slals0_
#define SLALSA   slalsa_
#define SLALSD   slalsd_
#define SLAMC3   slamc3_
#define SLAMCH   slamch_
#define SLAMRG   slamrg_
#define SLANEG   slaneg_
#define SLANGB   slangb_
#define SLANGE   slange_
#define SLANGT   slangt_
#define SLANHS   slanhs_
#define SLANSB   slansb_
#define SLANSF   slansf_
#define SLANSP   slansp_
#define SLANST   slanst_
#define SLANSY   slansy_
#define SLANTB   slantb_
#define SLANTP   slantp_
#define SLANTR   slantr_
#define SLANV2   slanv2_
#define SLAPLL   slapll_
#define SLAPMR   slapmr_
#define SLAPMT   slapmt_
#define SLAPY2   slapy2_
#define SLAPY3   slapy3_
#define SLAQGB   slaqgb_
#define SLAQGE   slaqge_
#define SLAQP2   slaqp2_
#define SLAQPS   slaqps_
#define SLAQR0   slaqr0_
#define SLAQR1   slaqr1_
#define SLAQR2   slaqr2_
#define SLAQR3   slaqr3_
#define SLAQR4   slaqr4_
#define SLAQR5   slaqr5_
#define SLAQSB   slaqsb_
#define SLAQSP   slaqsp_
#define SLAQSY   slaqsy_
#define SLAQTR   slaqtr_
#define SLAR1V   slar1v_
#define SLAR2V   slar2v_
#define SLARF    slarf_
#define SLARFB   slarfb_
#define SLARFG   slarfg_
#define SLARFGP  slarfgp_
#define SLARFT   slarft_
#define SLARFX   slarfx_
#define SLARGV   slargv_
#define SLARNV   slarnv_
#define SLARRA   slarra_
#define SLARRB   slarrb_
#define SLARRC   slarrc_
#define SLARRD   slarrd_
#define SLARRE   slarre_
#define SLARRF   slarrf_
#define SLARRJ   slarrj_
#define SLARRK   slarrk_
#define SLARRR   slarrr_
#define SLARRV   slarrv_
#define SLARTG   slartg_
#define SLARTGP  slartgp_
#define SLARTGS  slartgs_
#define SLARTV   slartv_
#define SLARUV   slaruv_
#define SLARZ    slarz_
#define SLARZB   slarzb_
#define SLARZT   slarzt_
#define SLAS2    slas2_
#define SLASCL   slascl_
#define SLASD0   slasd0_
#define SLASD1   slasd1_
#define SLASD2   slasd2_
#define SLASD3   slasd3_
#define SLASD4   slasd4_
#define SLASD5   slasd5_
#define SLASD6   slasd6_
#define SLASD7   slasd7_
#define SLASD8   slasd8_
#define SLASDA   slasda_
#define SLASDQ   slasdq_
#define SLASDT   slasdt_
#define SLASET   slaset_
#define SLASQ1   slasq1_
#define SLASQ2   slasq2_
#define SLASQ3   slasq3_
#define SLASQ4   slasq4_
#define SLASQ5   slasq5_
#define SLASQ6   slasq6_
#define SLASR    slasr_
#define SLASRT   slasrt_
#define SLASSQ   slassq_
#define SLASV2   slasv2_
#define SLASWP   slaswp_
#define SLASY2   slasy2_
#define SLASYF   slasyf_
#define SLATBS   slatbs_
#define SLATDF   slatdf_
#define SLATPS   slatps_
#define SLATRD   slatrd_
#define SLATRS   slatrs_
#define SLATRZ   slatrz_
#define SLATZM   slatzm_
#define SLAUU2   slauu2_
#define SLAUUM   slauum_
#define SOPGTR   sopgtr_
#define SOPMTR   sopmtr_
#define SORBDB   sorbdb_
#define SORCSD   sorcsd_
#define SORG2L   sorg2l_
#define SORG2R   sorg2r_
#define SORGBR   sorgbr_
#define SORGHR   sorghr_
#define SORGL2   sorgl2_
#define SORGLQ   sorglq_
#define SORGQL   sorgql_
#define SORGQR   sorgqr_
#define SORGR2   sorgr2_
#define SORGRQ   sorgrq_
#define SORGTR   sorgtr_
#define SORM2L   sorm2l_
#define SORM2R   sorm2r_
#define SORMBR   sormbr_
#define SORMHR   sormhr_
#define SORML2   sorml2_
#define SORMLQ   sormlq_
#define SORMQL   sormql_
#define SORMQR   sormqr_
#define SORMR2   sormr2_
#define SORMR3   sormr3_
#define SORMRQ   sormrq_
#define SORMRZ   sormrz_
#define SORMTR   sormtr_
#define SPBCON   spbcon_
#define SPBEQU   spbequ_
#define SPBRFS   spbrfs_
#define SPBSTF   spbstf_
#define SPBSV    spbsv_
#define SPBSVX   spbsvx_
#define SPBTF2   spbtf2_
#define SPBTRF   spbtrf_
#define SPBTRS   spbtrs_
#define SPFTRF   spftrf_
#define SPFTRI   spftri_
#define SPFTRS   spftrs_
#define SPOCON   spocon_
#define SPOEQU   spoequ_
#define SPOEQUB  spoequb_
#define SPORFS   sporfs_
#define SPOSV    sposv_
#define SPOSVX   sposvx_
#define SPOTF2   spotf2_
#define SPOTRF   spotrf_
#define SPOTRI   spotri_
#define SPOTRS   spotrs_
#define SPPCON   sppcon_
#define SPPEQU   sppequ_
#define SPPRFS   spprfs_
#define SPPSV    sppsv_
#define SPPSVX   sppsvx_
#define SPPTRF   spptrf_
#define SPPTRI   spptri_
#define SPPTRS   spptrs_
#define SPSTF2   spstf2_
#define SPSTRF   spstrf_
#define SPTCON   sptcon_
#define SPTEQR   spteqr_
#define SPTRFS   sptrfs_
#define SPTSV    sptsv_
#define SPTSVX   sptsvx_
#define SPTTRF   spttrf_
#define SPTTRS   spttrs_
#define SPTTS2   sptts2_
#define SRSCL    srscl_
#define SSBEV    ssbev_
#define SSBEVD   ssbevd_
#define SSBEVX   ssbevx_
#define SSBGST   ssbgst_
#define SSBGV    ssbgv_
#define SSBGVD   ssbgvd_
#define SSBGVX   ssbgvx_
#define SSBTRD   ssbtrd_
#define SSFRK    ssfrk_
#define SSPCON   sspcon_
#define SSPEV    sspev_
#define SSPEVD   sspevd_
#define SSPEVX   sspevx_
#define SSPGST   sspgst_
#define SSPGV    sspgv_
#define SSPGVD   sspgvd_
#define SSPGVX   sspgvx_
#define SSPRFS   ssprfs_
#define SSPSV    sspsv_
#define SSPSVX   sspsvx_
#define SSPTRD   ssptrd_
#define SSPTRF   ssptrf_
#define SSPTRI   ssptri_
#define SSPTRS   ssptrs_
#define SSTEBZ   sstebz_
#define SSTEDC   sstedc_
#define SSTEGR   sstegr_
#define SSTEIN   sstein_
#define SSTEMR   sstemr_
#define SSTEQR   ssteqr_
#define SSTERF   ssterf_
#define SSTEV    sstev_
#define SSTEVD   sstevd_
#define SSTEVR   sstevr_
#define SSTEVX   sstevx_
#define SSYCON   ssycon_
#define SSYCONV  ssyconv_
#define SSYEQUB  ssyequb_
#define SSYEV    ssyev_
#define SSYEVD   ssyevd_
#define SSYEVR   ssyevr_
#define SSYEVX   ssyevx_
#define SSYGS2   ssygs2_
#define SSYGST   ssygst_
#define SSYGV    ssygv_
#define SSYGVD   ssygvd_
#define SSYGVX   ssygvx_
#define SSYRFS   ssyrfs_
#define SSYSV    ssysv_
#define SSYSVX   ssysvx_
#define SSYSWAPR ssyswapr_
#define SSYTD2   ssytd2_
#define SSYTF2   ssytf2_
#define SSYTRD   ssytrd_
#define SSYTRF   ssytrf_
#define SSYTRI   ssytri_
#define SSYTRI2  ssytri2_
#define SSYTRI2X ssytri2x_
#define SSYTRS   ssytrs_
#define SSYTRS2  ssytrs2_
#define STBCON   stbcon_
#define STBRFS   stbrfs_
#define STBTRS   stbtrs_
#define STFSM    stfsm_
#define STFTRI   stftri_
#define STFTTP   stfttp_
#define STFTTR   stfttr_
#define STGEVC   stgevc_
#define STGEX2   stgex2_
#define STGEXC   stgexc_
#define STGSEN   stgsen_
#define STGSJA   stgsja_
#define STGSNA   stgsna_
#define STGSY2   stgsy2_
#define STGSYL   stgsyl_
#define STPCON   stpcon_
#define STPMQRT  stpmqrt_
#define STPQRT   stpqrt_
#define STPQRT2  stpqrt2_
#define STPRFB   stprfb_
#define STPRFS   stprfs_
#define STPTRI   stptri_
#define STPTRS   stptrs_
#define STPTTF   stpttf_
#define STPTTR   stpttr_
#define STRCON   strcon_
#define STREVC   strevc_
#define STREXC   strexc_
#define STRRFS   strrfs_
#define STRSEN   strsen_
#define STRSNA   strsna_
#define STRSYL   strsyl_
#define STRTI2   strti2_
#define STRTRI   strtri_
#define STRTRS   strtrs_
#define STRTTF   strttf_
#define STRTTP   strttp_
#define STZRQF   stzrqf_
#define STZRZF   stzrzf_
#define XERBLA_ARRAY xerbla_array_
#define ZBBCSD   zbbcsd_
#define ZBDSQR   zbdsqr_
#define ZCGESV   zcgesv_
#define ZCPOSV   zcposv_
#define ZDRSCL   zdrscl_
#define ZGBBRD   zgbbrd_
#define ZGBCON   zgbcon_
#define ZGBEQU   zgbequ_
#define ZGBEQUB  zgbequb_
#define ZGBRFS   zgbrfs_
#define ZGBSV    zgbsv_
#define ZGBSVX   zgbsvx_
#define ZGBTF2   zgbtf2_
#define ZGBTRF   zgbtrf_
#define ZGBTRS   zgbtrs_
#define ZGEBAK   zgebak_
#define ZGEBAL   zgebal_
#define ZGEBD2   zgebd2_
#define ZGEBRD   zgebrd_
#define ZGECON   zgecon_
#define ZGEEQU   zgeequ_
#define ZGEEQUB  zgeequb_
#define ZGEES    zgees_
#define ZGEESX   zgeesx_
#define ZGEEV    zgeev_
#define ZGEEVX   zgeevx_
#define ZGEGS    zgegs_
#define ZGEGV    zgegv_
#define ZGEHD2   zgehd2_
#define ZGEHRD   zgehrd_
#define ZGELQ2   zgelq2_
#define ZGELQF   zgelqf_
#define ZGELS    zgels_
#define ZGELSD   zgelsd_
#define ZGELSS   zgelss_
#define ZGELSX   zgelsx_
#define ZGELSY   zgelsy_
#define ZGEMQRT  zgemqrt_
#define ZGEQL2   zgeql2_
#define ZGEQLF   zgeqlf_
#define ZGEQP3   zgeqp3_
#define ZGEQPF   zgeqpf_
#define ZGEQR2   zgeqr2_
#define ZGEQR2P  zgeqr2p_
#define ZGEQRF   zgeqrf_
#define ZGEQRFP  zgeqrfp_
#define ZGEQRT   zgeqrt_
#define ZGEQRT2  zgeqrt2_
#define ZGEQRT3  zgeqrt3_
#define ZGERFS   zgerfs_
#define ZGERQ2   zgerq2_
#define ZGERQF   zgerqf_
#define ZGESC2   zgesc2_
#define ZGESDD   zgesdd_
#define ZGESV    zgesv_
#define ZGESVD   zgesvd_
#define ZGESVX   zgesvx_
#define ZGETC2   zgetc2_
#define ZGETF2   zgetf2_
#define ZGETRF   zgetrf_
#define ZGETRI   zgetri_
#define ZGETRS   zgetrs_
#define ZGGBAK   zggbak_
#define ZGGBAL   zggbal_
#define ZGGES    zgges_
#define ZGGESX   zggesx_
#define ZGGEV    zggev_
#define ZGGEVX   zggevx_
#define ZGGGLM   zggglm_
#define ZGGHRD   zgghrd_
#define ZGGLSE   zgglse_
#define ZGGQRF   zggqrf_
#define ZGGRQF   zggrqf_
#define ZGGSVD   zggsvd_
#define ZGGSVP   zggsvp_
#define ZGTCON   zgtcon_
#define ZGTRFS   zgtrfs_
#define ZGTSV    zgtsv_
#define ZGTSVX   zgtsvx_
#define ZGTTRF   zgttrf_
#define ZGTTRS   zgttrs_
#define ZGTTS2   zgtts2_
#define ZHBEV    zhbev_
#define ZHBEVD   zhbevd_
#define ZHBEVX   zhbevx_
#define ZHBGST   zhbgst_
#define ZHBGV    zhbgv_
#define ZHBGVD   zhbgvd_
#define ZHBGVX   zhbgvx_
#define ZHBTRD   zhbtrd_
#define ZHECON   zhecon_
#define ZHEEQUB  zheequb_
#define ZHEEV    zheev_
#define ZHEEVD   zheevd_
#define ZHEEVR   zheevr_
#define ZHEEVX   zheevx_
#define ZHEGS2   zhegs2_
#define ZHEGST   zhegst_
#define ZHEGV    zhegv_
#define ZHEGVD   zhegvd_
#define ZHEGVX   zhegvx_
#define ZHERFS   zherfs_
#define ZHESV    zhesv_
#define ZHESVX   zhesvx_
#define ZHESWAPR zheswapr_
#define ZHETD2   zhetd2_
#define ZHETF2   zhetf2_
#define ZHETRD   zhetrd_
#define ZHETRF   zhetrf_
#define ZHETRI   zhetri_
#define ZHETRI2  zhetri2_
#define ZHETRI2X zhetri2x_
#define ZHETRS   zhetrs_
#define ZHETRS2  zhetrs2_
#define ZHFRK    zhfrk_
#define ZHGEQZ   zhgeqz_
#define ZHPCON   zhpcon_
#define ZHPEV    zhpev_
#define ZHPEVD   zhpevd_
#define ZHPEVX   zhpevx_
#define ZHPGST   zhpgst_
#define ZHPGV    zhpgv_
#define ZHPGVD   zhpgvd_
#define ZHPGVX   zhpgvx_
#define ZHPRFS   zhprfs_
#define ZHPSV    zhpsv_
#define ZHPSVX   zhpsvx_
#define ZHPTRD   zhptrd_
#define ZHPTRF   zhptrf_
#define ZHPTRI   zhptri_
#define ZHPTRS   zhptrs_
#define ZHSEIN   zhsein_
#define ZHSEQR   zhseqr_
#define ZLABRD   zlabrd_
#define ZLACGV   zlacgv_
#define ZLACN2   zlacn2_
#define ZLACON   zlacon_
#define ZLACP2   zlacp2_
#define ZLACPY   zlacpy_
#define ZLACRM   zlacrm_
#define ZLACRT   zlacrt_
#define ZLADIV   zladiv_
#define ZLAED0   zlaed0_
#define ZLAED7   zlaed7_
#define ZLAED8   zlaed8_
#define ZLAEIN   zlaein_
#define ZLAESY   zlaesy_
#define ZLAEV2   zlaev2_
#define ZLAG2C   zlag2c_
#define ZLAGS2   zlags2_
#define ZLAGTM   zlagtm_
#define ZLAHEF   zlahef_
#define ZLAHQR   zlahqr_
#define ZLAHR2   zlahr2_
#define ZLAHRD   zlahrd_
#define ZLAIC1   zlaic1_
#define ZLALS0   zlals0_
#define ZLALSA   zlalsa_
#define ZLALSD   zlalsd_
#define ZLANGB   zlangb_
#define ZLANGE   zlange_
#define ZLANGT   zlangt_
#define ZLANHB   zlanhb_
#define ZLANHE   zlanhe_
#define ZLANHF   zlanhf_
#define ZLANHP   zlanhp_
#define ZLANHS   zlanhs_
#define ZLANHT   zlanht_
#define ZLANSB   zlansb_
#define ZLANSP   zlansp_
#define ZLANSY   zlansy_
#define ZLANTB   zlantb_
#define ZLANTP   zlantp_
#define ZLANTR   zlantr_
#define ZLAPLL   zlapll_
#define ZLAPMR   zlapmr_
#define ZLAPMT   zlapmt_
#define ZLAQGB   zlaqgb_
#define ZLAQGE   zlaqge_
#define ZLAQHB   zlaqhb_
#define ZLAQHE   zlaqhe_
#define ZLAQHP   zlaqhp_
#define ZLAQP2   zlaqp2_
#define ZLAQPS   zlaqps_
#define ZLAQR0   zlaqr0_
#define ZLAQR1   zlaqr1_
#define ZLAQR2   zlaqr2_
#define ZLAQR3   zlaqr3_
#define ZLAQR4   zlaqr4_
#define ZLAQR5   zlaqr5_
#define ZLAQSB   zlaqsb_
#define ZLAQSP   zlaqsp_
#define ZLAQSY   zlaqsy_
#define ZLAR1V   zlar1v_
#define ZLAR2V   zlar2v_
#define ZLARCM   zlarcm_
#define ZLARF    zlarf_
#define ZLARFB   zlarfb_
#define ZLARFG   zlarfg_
#define ZLARFGP  zlarfgp_
#define ZLARFT   zlarft_
#define ZLARFX   zlarfx_
#define ZLARGV   zlargv_
#define ZLARNV   zlarnv_
#define ZLARRV   zlarrv_
#define ZLARTG   zlartg_
#define ZLARTV   zlartv_
#define ZLARZ    zlarz_
#define ZLARZB   zlarzb_
#define ZLARZT   zlarzt_
#define ZLASCL   zlascl_
#define ZLASET   zlaset_
#define ZLASR    zlasr_
#define ZLASSQ   zlassq_
#define ZLASWP   zlaswp_
#define ZLASYF   zlasyf_
#define ZLAT2C   zlat2c_
#define ZLATBS   zlatbs_
#define ZLATDF   zlatdf_
#define ZLATPS   zlatps_
#define ZLATRD   zlatrd_
#define ZLATRS   zlatrs_
#define ZLATRZ   zlatrz_
#define ZLATZM   zlatzm_
#define ZLAUU2   zlauu2_
#define ZLAUUM   zlauum_
#define ZPBCON   zpbcon_
#define ZPBEQU   zpbequ_
#define ZPBRFS   zpbrfs_
#define ZPBSTF   zpbstf_
#define ZPBSV    zpbsv_
#define ZPBSVX   zpbsvx_
#define ZPBTF2   zpbtf2_
#define ZPBTRF   zpbtrf_
#define ZPBTRS   zpbtrs_
#define ZPFTRF   zpftrf_
#define ZPFTRI   zpftri_
#define ZPFTRS   zpftrs_
#define ZPOCON   zpocon_
#define ZPOEQU   zpoequ_
#define ZPOEQUB  zpoequb_
#define ZPORFS   zporfs_
#define ZPOSV    zposv_
#define ZPOSVX   zposvx_
#define ZPOTF2   zpotf2_
#define ZPOTRF   zpotrf_
#define ZPOTRI   zpotri_
#define ZPOTRS   zpotrs_
#define ZPPCON   zppcon_
#define ZPPEQU   zppequ_
#define ZPPRFS   zpprfs_
#define ZPPSV    zppsv_
#define ZPPSVX   zppsvx_
#define ZPPTRF   zpptrf_
#define ZPPTRI   zpptri_
#define ZPPTRS   zpptrs_
#define ZPSTF2   zpstf2_
#define ZPSTRF   zpstrf_
#define ZPTCON   zptcon_
#define ZPTEQR   zpteqr_
#define ZPTRFS   zptrfs_
#define ZPTSV    zptsv_
#define ZPTSVX   zptsvx_
#define ZPTTRF   zpttrf_
#define ZPTTRS   zpttrs_
#define ZPTTS2   zptts2_
#define ZROT     zrot_
#define ZSPCON   zspcon_
#define ZSPMV    zspmv_
#define ZSPR     zspr_
#define ZSPRFS   zsprfs_
#define ZSPSV    zspsv_
#define ZSPSVX   zspsvx_
#define ZSPTRF   zsptrf_
#define ZSPTRI   zsptri_
#define ZSPTRS   zsptrs_
#define ZSTEDC   zstedc_
#define ZSTEGR   zstegr_
#define ZSTEIN   zstein_
#define ZSTEMR   zstemr_
#define ZSTEQR   zsteqr_
#define ZSYCON   zsycon_
#define ZSYCONV  zsyconv_
#define ZSYEQUB  zsyequb_
#define ZSYMV    zsymv_
#define ZSYR     zsyr_
#define ZSYRFS   zsyrfs_
#define ZSYSV    zsysv_
#define ZSYSVX   zsysvx_
#define ZSYSWAPR zsyswapr_
#define ZSYTF2   zsytf2_
#define ZSYTRF   zsytrf_
#define ZSYTRI   zsytri_
#define ZSYTRI2  zsytri2_
#define ZSYTRI2X zsytri2x_
#define ZSYTRS   zsytrs_
#define ZSYTRS2  zsytrs2_
#define ZTBCON   ztbcon_
#define ZTBRFS   ztbrfs_
#define ZTBTRS   ztbtrs_
#define ZTFSM    ztfsm_
#define ZTFTRI   ztftri_
#define ZTFTTP   ztfttp_
#define ZTFTTR   ztfttr_
#define ZTGEVC   ztgevc_
#define ZTGEX2   ztgex2_
#define ZTGEXC   ztgexc_
#define ZTGSEN   ztgsen_
#define ZTGSJA   ztgsja_
#define ZTGSNA   ztgsna_
#define ZTGSY2   ztgsy2_
#define ZTGSYL   ztgsyl_
#define ZTPCON   ztpcon_
#define ZTPMQRT  ztpmqrt_
#define ZTPQRT   ztpqrt_
#define ZTPQRT2  ztpqrt2_
#define ZTPRFB   ztprfb_
#define ZTPRFS   ztprfs_
#define ZTPTRI   ztptri_
#define ZTPTRS   ztptrs_
#define ZTPTTF   ztpttf_
#define ZTPTTR   ztpttr_
#define ZTRCON   ztrcon_
#define ZTREVC   ztrevc_
#define ZTREXC   ztrexc_
#define ZTRRFS   ztrrfs_
#define ZTRSEN   ztrsen_
#define ZTRSNA   ztrsna_
#define ZTRSYL   ztrsyl_
#define ZTRTI2   ztrti2_
#define ZTRTRI   ztrtri_
#define ZTRTRS   ztrtrs_
#define ZTRTTF   ztrttf_
#define ZTRTTP   ztrttp_
#define ZTZRQF   ztzrqf_
#define ZTZRZF   ztzrzf_
#define ZUNBDB   zunbdb_
#define ZUNCSD   zuncsd_
#define ZUNG2L   zung2l_
#define ZUNG2R   zung2r_
#define ZUNGBR   zungbr_
#define ZUNGHR   zunghr_
#define ZUNGL2   zungl2_
#define ZUNGLQ   zunglq_
#define ZUNGQL   zungql_
#define ZUNGQR   zungqr_
#define ZUNGR2   zungr2_
#define ZUNGRQ   zungrq_
#define ZUNGTR   zungtr_
#define ZUNM2L   zunm2l_
#define ZUNM2R   zunm2r_
#define ZUNMBR   zunmbr_
#define ZUNMHR   zunmhr_
#define ZUNML2   zunml2_
#define ZUNMLQ   zunmlq_
#define ZUNMQL   zunmql_
#define ZUNMQR   zunmqr_
#define ZUNMR2   zunmr2_
#define ZUNMR3   zunmr3_
#define ZUNMRQ   zunmrq_
#define ZUNMRZ   zunmrz_
#define ZUNMTR   zunmtr_
#define ZUPGTR   zupgtr_
#define ZUPMTR   zupmtr_
#endif

    void FC_GLOBAL(cbbcsd,CBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, lapack_complex_float* u1, blasint* ldu1, lapack_complex_float* u2, blasint* ldu2, lapack_complex_float* v1t, blasint* ldv1t, lapack_complex_float* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cbdsqr,CBDSQR)(char* uplo, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, float* d, float* e, lapack_complex_float* vt, blasint* ldvt, lapack_complex_float* u, blasint* ldu, lapack_complex_float* c, blasint* ldc, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cgbbrd,CGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, lapack_complex_float* ab, blasint* ldab, float* d, float* e, lapack_complex_float* q, blasint* ldq, lapack_complex_float* pt, blasint* ldpt, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_vect);

    void FC_GLOBAL(cgbcon,CGBCON)(char* norm, blasint* n, blasint* kl, blasint* ku, lapack_complex_float* ab, blasint* ldab, blasint* ipiv, float* anorm, float* rcond, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(cgbequ,CGBEQU)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_float* ab, blasint* ldab, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info);

    void FC_GLOBAL(cgbequb,CGBEQUB)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_float* ab, blasint* ldab, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info);

    void FC_GLOBAL(cgbrfs,CGBRFS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* afb, blasint* ldafb, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cgbsv,CGBSV)(blasint* n, blasint* kl, blasint* ku, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(cgbsvx,CGBSVX)(char* fact, char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* afb, blasint* ldafb, blasint* ipiv, char* equed, float* r, float* c, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(cgbtf2,CGBTF2)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_float* ab, blasint* ldab, blasint* ipiv, blasint* info);

    void FC_GLOBAL(cgbtrf,CGBTRF)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_float* ab, blasint* ldab, blasint* ipiv, blasint* info);

    void FC_GLOBAL(cgbtrs,CGBTRS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cgebak,CGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, float* scale, blasint* m, lapack_complex_float* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(cgebal,CGEBAL)(char* job, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info, flexiblas_fortran_charlen_t len_job);

    void FC_GLOBAL(cgebd2,CGEBD2)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* d, float* e, lapack_complex_float* tauq, lapack_complex_float* taup, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cgebrd,CGEBRD)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* d, float* e, lapack_complex_float* tauq, lapack_complex_float* taup, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cgecon,CGECON)(char* norm, blasint* n, lapack_complex_float* a, blasint* lda, float* anorm, float* rcond, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(cgeequ,CGEEQU)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info);

    void FC_GLOBAL(cgeequb,CGEEQUB)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info);

    void FC_GLOBAL(cgees,CGEES)(char* jobvs, char* sort, blaslogical* selectfunc, blasint* n, lapack_complex_float* a, blasint* lda, blasint* sdim, lapack_complex_float* w, lapack_complex_float* vs, blasint* ldvs, lapack_complex_float* work, blasint* lwork, float* rwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

    void FC_GLOBAL(cgeesx,CGEESX)(char* jobvs, char* sort, blaslogical* selectfunc, char* sense, blasint* n, lapack_complex_float* a, blasint* lda, blasint* sdim, lapack_complex_float* w, lapack_complex_float* vs, blasint* ldvs, float* rconde, float* rcondv, lapack_complex_float* work, blasint* lwork, float* rwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(cgeev,CGEEV)(char* jobvl, char* jobvr, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* w, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(cgeevx,CGEEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* w, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(cgegs,CGEGS)(char* jobvsl, char* jobvsr, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* alpha, lapack_complex_float* beta, lapack_complex_float* vsl, blasint* ldvsl, lapack_complex_float* vsr, blasint* ldvsr, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    void FC_GLOBAL(cgegv,CGEGV)(char* jobvl, char* jobvr, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* alpha, lapack_complex_float* beta, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(cgehd2,CGEHD2)(blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cgehrd,CGEHRD)(blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cgelq2,CGELQ2)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cgelqf,CGELQF)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cgels,CGELS)(char* trans, blasint* m, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cgelsd,CGELSD)(blasint* m, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(cgelss,CGELSS)(blasint* m, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info);

    void FC_GLOBAL(cgelsx,CGELSX)(blasint* m, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* jpvt, float* rcond, blasint* rank_bn, lapack_complex_float* work, float* rwork, blasint* info);

    void FC_GLOBAL(cgelsy,CGELSY)(blasint* m, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* jpvt, float* rcond, blasint* rank_bn, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info);

    void FC_GLOBAL(cgemqrt,CGEMQRT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* nb, lapack_complex_float* v, blasint* ldv, lapack_complex_float* t, blasint* ldt, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cgeql2,CGEQL2)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cgeqlf,CGEQLF)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cgeqp3,CGEQP3)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, blasint* jpvt, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info);

    void FC_GLOBAL(cgeqpf,CGEQPF)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, blasint* jpvt, lapack_complex_float* tau, lapack_complex_float* work, float* rwork, blasint* info);

    void FC_GLOBAL(cgeqr2,CGEQR2)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cgeqr2p,CGEQR2P)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cgeqrf,CGEQRF)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cgeqrfp,CGEQRFP)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cgeqrt,CGEQRT)(blasint* m, blasint* n, blasint* nb, lapack_complex_float* a, blasint* lda, lapack_complex_float* t, blasint* ldt, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cgeqrt2,CGEQRT2)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(cgeqrt3,CGEQRT3)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(cgerfs,CGERFS)(char* trans, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* af, blasint* ldaf, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cgerq2,CGERQ2)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cgerqf,CGERQF)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cgesc2,CGESC2)(blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* rhs, blasint* ipiv, blasint* jpiv, float* scale);

    void FC_GLOBAL(cgesdd,CGESDD)(char* jobz, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* s, lapack_complex_float* u, blasint* ldu, lapack_complex_float* vt, blasint* ldvt, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz);

    void FC_GLOBAL(cgesv,CGESV)(blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(cgesvd,CGESVD)(char* jobu, char* jobvt, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* s, lapack_complex_float* u, blasint* ldu, lapack_complex_float* vt, blasint* ldvt, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt);

    void FC_GLOBAL(cgesvx,CGESVX)(char* fact, char* trans, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* af, blasint* ldaf, blasint* ipiv, char* equed, float* r, float* c, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(cgetc2,CGETC2)(blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, blasint* jpiv, blasint* info);

    void FC_GLOBAL(cgetf2,CGETF2)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, blasint* info);

    void FC_GLOBAL(cgetrf,CGETRF)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, blasint* info);

    void FC_GLOBAL(cgetri,CGETRI)(blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cgetrs,CGETRS)(char* trans, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cggbak,CGGBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, float* lscale, float* rscale, blasint* m, lapack_complex_float* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(cggbal,CGGBAL)(char* job, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* work, blasint* info, flexiblas_fortran_charlen_t len_job);

    void FC_GLOBAL(cgges,CGGES)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* sdim, lapack_complex_float* alpha, lapack_complex_float* beta, lapack_complex_float* vsl, blasint* ldvsl, lapack_complex_float* vsr, blasint* ldvsr, lapack_complex_float* work, blasint* lwork, float* rwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort);

    void FC_GLOBAL(cggesx,CGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* sdim, lapack_complex_float* alpha, lapack_complex_float* beta, lapack_complex_float* vsl, blasint* ldvsl, lapack_complex_float* vsr, blasint* ldvsr, float* rconde, float* rcondv, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(cggev,CGGEV)(char* jobvl, char* jobvr, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* alpha, lapack_complex_float* beta, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(cggevx,CGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* alpha, lapack_complex_float* beta, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* abnrm, float* bbnrm, float* rconde, float* rcondv, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(cggglm,CGGGLM)(blasint* n, blasint* m, blasint* p, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* d, lapack_complex_float* x, lapack_complex_float* y, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cgghrd,CGGHRD)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* q, blasint* ldq, lapack_complex_float* z, blasint* ldz, blasint* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(cgglse,CGGLSE)(blasint* m, blasint* n, blasint* p, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* c, lapack_complex_float* d, lapack_complex_float* x, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cggqrf,CGGQRF)(blasint* n, blasint* m, blasint* p, lapack_complex_float* a, blasint* lda, lapack_complex_float* taua, lapack_complex_float* b, blasint* ldb, lapack_complex_float* taub, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cggrqf,CGGRQF)(blasint* m, blasint* p, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* taua, lapack_complex_float* b, blasint* ldb, lapack_complex_float* taub, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cggsvd,CGGSVD)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* n, blasint* p, blasint* k, blasint* l, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, float* alpha, float* beta, lapack_complex_float* u, blasint* ldu, lapack_complex_float* v, blasint* ldv, lapack_complex_float* q, blasint* ldq, lapack_complex_float* work, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(cggsvp,CGGSVP)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, float* tola, float* tolb, blasint* k, blasint* l, lapack_complex_float* u, blasint* ldu, lapack_complex_float* v, blasint* ldv, lapack_complex_float* q, blasint* ldq, blasint* iwork, float* rwork, lapack_complex_float* tau, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(cgtcon,CGTCON)(char* norm, blasint* n, lapack_complex_float* dl, lapack_complex_float* d, lapack_complex_float* du, lapack_complex_float* du2, blasint* ipiv, float* anorm, float* rcond, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(cgtrfs,CGTRFS)(char* trans, blasint* n, blasint* nrhs, lapack_complex_float* dl, lapack_complex_float* d, lapack_complex_float* du, lapack_complex_float* dlf, lapack_complex_float* df, lapack_complex_float* duf, lapack_complex_float* du2, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cgtsv,CGTSV)(blasint* n, blasint* nrhs, lapack_complex_float* dl, lapack_complex_float* d, lapack_complex_float* du, lapack_complex_float* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(cgtsvx,CGTSVX)(char* fact, char* trans, blasint* n, blasint* nrhs, lapack_complex_float* dl, lapack_complex_float* d, lapack_complex_float* du, lapack_complex_float* dlf, lapack_complex_float* df, lapack_complex_float* duf, lapack_complex_float* du2, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cgttrf,CGTTRF)(blasint* n, lapack_complex_float* dl, lapack_complex_float* d, lapack_complex_float* du, lapack_complex_float* du2, blasint* ipiv, blasint* info);

    void FC_GLOBAL(cgttrs,CGTTRS)(char* trans, blasint* n, blasint* nrhs, lapack_complex_float* dl, lapack_complex_float* d, lapack_complex_float* du, lapack_complex_float* du2, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cgtts2,CGTTS2)(blasint* itrans, blasint* n, blasint* nrhs, lapack_complex_float* dl, lapack_complex_float* d, lapack_complex_float* du, lapack_complex_float* du2, blasint* ipiv, lapack_complex_float* b, blasint* ldb);

    void FC_GLOBAL(chbev,CHBEV)(char* jobz, char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chbevd,CHBEVD)(char* jobz, char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chbevx,CHBEVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* q, blasint* ldq, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, float* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chbgst,CHBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* bb, blasint* ldbb, lapack_complex_float* x, blasint* ldx, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chbgv,CHBGV)(char* jobz, char* uplo, blasint* n, blasint* ka, blasint* kb, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* bb, blasint* ldbb, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chbgvd,CHBGVD)(char* jobz, char* uplo, blasint* n, blasint* ka, blasint* kb, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* bb, blasint* ldbb, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chbgvx,CHBGVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* ka, blasint* kb, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* bb, blasint* ldbb, lapack_complex_float* q, blasint* ldq, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, float* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chbtrd,CHBTRD)(char* vect, char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, float* d, float* e, lapack_complex_float* q, blasint* ldq, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(checon,CHECON)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, float* anorm, float* rcond, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cheequb,CHEEQUB)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* s, float* scond, float* amax, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cheev,CHEEV)(char* jobz, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* w, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cheevd,CHEEVD)(char* jobz, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* w, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cheevr,CHEEVR)(char* jobz, char* range, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, lapack_complex_float* z, blasint* ldz, blasint* isuppz, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cheevx,CHEEVX)(char* jobz, char* range, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chegs2,CHEGS2)(blasint* itype, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chegst,CHEGST)(blasint* itype, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chegv,CHEGV)(blasint* itype, char* jobz, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, float* w, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chegvd,CHEGVD)(blasint* itype, char* jobz, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, float* w, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chegvx,CHEGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cherfs,CHERFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* af, blasint* ldaf, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chesv,CHESV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chesvx,CHESVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* af, blasint* ldaf, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cheswapr,CHESWAPR)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* i1, blasint* i2, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chetd2,CHETD2)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* d, float* e, lapack_complex_float* tau, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chetf2,CHETF2)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chetrd,CHETRD)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* d, float* e, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chetrf,CHETRF)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chetri,CHETRI)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chetri2,CHETRI2)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chetri2x,CHETRI2X)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* nb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chetrs,CHETRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chetrs2,CHETRS2)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chfrk,CHFRK)(char* transr, char* uplo, char* trans, blasint* n, blasint* k, float* alpha, lapack_complex_float* a, blasint* lda, float* beta, lapack_complex_float* c, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(chgeqz,CHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* h, blasint* ldh, lapack_complex_float* t, blasint* ldt, lapack_complex_float* alpha, lapack_complex_float* beta, lapack_complex_float* q, blasint* ldq, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    char FC_GLOBAL_(chla_transtype,CHLA_TRANSTYPE)(blasint* trans);

    void FC_GLOBAL(chpcon,CHPCON)(char* uplo, blasint* n, lapack_complex_float* ap, blasint* ipiv, float* anorm, float* rcond, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chpev,CHPEV)(char* jobz, char* uplo, blasint* n, lapack_complex_float* ap, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chpevd,CHPEVD)(char* jobz, char* uplo, blasint* n, lapack_complex_float* ap, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chpevx,CHPEVX)(char* jobz, char* range, char* uplo, blasint* n, lapack_complex_float* ap, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, float* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chpgst,CHPGST)(blasint* itype, char* uplo, blasint* n, lapack_complex_float* ap, lapack_complex_float* bp, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chpgv,CHPGV)(blasint* itype, char* jobz, char* uplo, blasint* n, lapack_complex_float* ap, lapack_complex_float* bp, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chpgvd,CHPGVD)(blasint* itype, char* jobz, char* uplo, blasint* n, lapack_complex_float* ap, lapack_complex_float* bp, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chpgvx,CHPGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, lapack_complex_float* ap, lapack_complex_float* bp, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, float* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chprfs,CHPRFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* afp, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chpsv,CHPSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chpsvx,CHPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* afp, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chptrd,CHPTRD)(char* uplo, blasint* n, lapack_complex_float* ap, float* d, float* e, lapack_complex_float* tau, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chptrf,CHPTRF)(char* uplo, blasint* n, lapack_complex_float* ap, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chptri,CHPTRI)(char* uplo, blasint* n, lapack_complex_float* ap, blasint* ipiv, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chptrs,CHPTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(chsein,CHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* selectfunc, blasint* n, lapack_complex_float* h, blasint* ldh, lapack_complex_float* w, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, blasint* mm, blasint* m, lapack_complex_float* work, float* rwork, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

    void FC_GLOBAL(chseqr,CHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* h, blasint* ldh, lapack_complex_float* w, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(clabrd,CLABRD)(blasint* m, blasint* n, blasint* nb, lapack_complex_float* a, blasint* lda, float* d, float* e, lapack_complex_float* tauq, lapack_complex_float* taup, lapack_complex_float* x, blasint* ldx, lapack_complex_float* y, blasint* ldy);

    void FC_GLOBAL(clacgv,CLACGV)(blasint* n, lapack_complex_float* x, blasint* incx);

    void FC_GLOBAL(clacn2,CLACN2)(blasint* n, lapack_complex_float* v, lapack_complex_float* x, float* est, blasint* kase, blasint* isave);

    void FC_GLOBAL(clacon,CLACON)(blasint* n, lapack_complex_float* v, lapack_complex_float* x, float* est, blasint* kase);

    void FC_GLOBAL(clacp2,CLACP2)(char* uplo, blasint* m, blasint* n, float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(clacpy,CLACPY)(char* uplo, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(clacrm,CLACRM)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* b, blasint* ldb, lapack_complex_float* c, blasint* ldc, float* rwork);

    void FC_GLOBAL(clacrt,CLACRT)(blasint* n, lapack_complex_float* cx, blasint* incx, lapack_complex_float* cy, blasint* incy, lapack_complex_float* c, lapack_complex_float* s);

    lapack_complex_float FC_GLOBAL(cladiv,CLADIV)(lapack_complex_float* x, lapack_complex_float* y);

    void FC_GLOBAL(claed0,CLAED0)(blasint* qsiz, blasint* n, float* d, float* e, lapack_complex_float* q, blasint* ldq, lapack_complex_float* qstore, blasint* ldqs, float* rwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(claed7,CLAED7)(blasint* n, blasint* cutpnt, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, lapack_complex_float* q, blasint* ldq, float* rho, blasint* indxq, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, lapack_complex_float* work, float* rwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(claed8,CLAED8)(blasint* k, blasint* n, blasint* qsiz, lapack_complex_float* q, blasint* ldq, float* d, float* rho, blasint* cutpnt, float* z, float* dlamda, lapack_complex_float* q2, blasint* ldq2, float* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* info);

    void FC_GLOBAL(claein,CLAEIN)(blaslogical* rightv, blaslogical* noinit, blasint* n, lapack_complex_float* h, blasint* ldh, lapack_complex_float* w, lapack_complex_float* v, lapack_complex_float* b, blasint* ldb, float* rwork, float* eps3, float* smlnum, blasint* info);

    void FC_GLOBAL(claesy,CLAESY)(lapack_complex_float* a, lapack_complex_float* b, lapack_complex_float* c, lapack_complex_float* rt1, lapack_complex_float* rt2, lapack_complex_float* evscal, lapack_complex_float* cs1, lapack_complex_float* sn1);

    void FC_GLOBAL(claev2,CLAEV2)(lapack_complex_float* a, lapack_complex_float* b, lapack_complex_float* c, float* rt1, float* rt2, float* cs1, lapack_complex_float* sn1);

    void FC_GLOBAL(clag2z,CLAG2Z)(blasint* m, blasint* n, lapack_complex_float* sa, blasint* ldsa, lapack_complex_double* a, blasint* lda, blasint* info);

    void FC_GLOBAL(clags2,CLAGS2)(blaslogical* upper, float* a1, lapack_complex_float* a2, float* a3, float* b1, lapack_complex_float* b2, float* b3, float* csu, lapack_complex_float* snu, float* csv, lapack_complex_float* snv, float* csq, lapack_complex_float* snq);

    void FC_GLOBAL(clagtm,CLAGTM)(char* trans, blasint* n, blasint* nrhs, float* alpha, lapack_complex_float* dl, lapack_complex_float* d, lapack_complex_float* du, lapack_complex_float* x, blasint* ldx, float* beta, lapack_complex_float* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(clahef,CLAHEF)(char* uplo, blasint* n, blasint* nb, blasint* kb, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(clahqr,CLAHQR)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* h, blasint* ldh, lapack_complex_float* w, blasint* iloz, blasint* ihiz, lapack_complex_float* z, blasint* ldz, blasint* info);

    void FC_GLOBAL(clahr2,CLAHR2)(blasint* n, blasint* k, blasint* nb, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* t, blasint* ldt, lapack_complex_float* y, blasint* ldy);

    void FC_GLOBAL(clahrd,CLAHRD)(blasint* n, blasint* k, blasint* nb, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* t, blasint* ldt, lapack_complex_float* y, blasint* ldy);

    void FC_GLOBAL(claic1,CLAIC1)(blasint* job, blasint* j, lapack_complex_float* x, float* sest, lapack_complex_float* w, lapack_complex_float* gamma, float* sestpr, lapack_complex_float* s, lapack_complex_float* c);

    void FC_GLOBAL(clals0,CLALS0)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, lapack_complex_float* b, blasint* ldb, lapack_complex_float* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* poles, float* difl, float* difr, float* z, blasint* k, float* c, float* s, float* rwork, blasint* info);

    void FC_GLOBAL(clalsa,CLALSA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* nrhs, lapack_complex_float* b, blasint* ldb, lapack_complex_float* bx, blasint* ldbx, float* u, blasint* ldu, float* vt, blasint* k, float* difl, float* difr, float* z, float* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, float* givnum, float* c, float* s, float* rwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(clalsd,CLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, lapack_complex_float* b, blasint* ldb, float* rcond, blasint* rank_bn, lapack_complex_float* work, float* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(clangb,CLANGB)(char* norm, blasint* n, blasint* kl, blasint* ku, lapack_complex_float* ab, blasint* ldab, float* work, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(clange,CLANGE)(char* norm, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(clangt,CLANGT)(char* norm, blasint* n, lapack_complex_float* dl, lapack_complex_float* d, lapack_complex_float* du, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(clanhb,CLANHB)(char* norm, char* uplo, blasint* n, blasint* k, lapack_complex_float* ab, blasint* ldab, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(clanhe,CLANHE)(char* norm, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(clanhf,CLANHF)(char* norm, char* transr, char* uplo, blasint* n, lapack_complex_float* a, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(clanhp,CLANHP)(char* norm, char* uplo, blasint* n, lapack_complex_float* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(clanhs,CLANHS)(char* norm, blasint* n, lapack_complex_float* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(clanht,CLANHT)(char* norm, blasint* n, float* d, lapack_complex_float* e, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(clansb,CLANSB)(char* norm, char* uplo, blasint* n, blasint* k, lapack_complex_float* ab, blasint* ldab, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(clansp,CLANSP)(char* norm, char* uplo, blasint* n, lapack_complex_float* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(clansy,CLANSY)(char* norm, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(clantb,CLANTB)(char* norm, char* uplo, char* diag, blasint* n, blasint* k, lapack_complex_float* ab, blasint* ldab, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    float FC_GLOBAL(clantp,CLANTP)(char* norm, char* uplo, char* diag, blasint* n, lapack_complex_float* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    float FC_GLOBAL(clantr,CLANTR)(char* norm, char* uplo, char* diag, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(clapll,CLAPLL)(blasint* n, lapack_complex_float* x, blasint* incx, lapack_complex_float* y, blasint* incy, float* ssmin);

    void FC_GLOBAL(clapmr,CLAPMR)(blaslogical* forwrd, blasint* m, blasint* n, lapack_complex_float* x, blasint* ldx, blasint* k);

    void FC_GLOBAL(clapmt,CLAPMT)(blaslogical* forwrd, blasint* m, blasint* n, lapack_complex_float* x, blasint* ldx, blasint* k);

    void FC_GLOBAL(claqgb,CLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_float* ab, blasint* ldab, float* r, float* c, float* rowcnd, float* colcnd, float* amax, char* equed, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(claqge,CLAQGE)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, char* equed, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(claqhb,CLAQHB)(char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(claqhe,CLAQHE)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(claqhp,CLAQHP)(char* uplo, blasint* n, lapack_complex_float* ap, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(claqp2,CLAQP2)(blasint* m, blasint* n, blasint* offset, lapack_complex_float* a, blasint* lda, blasint* jpvt, lapack_complex_float* tau, float* vn1, float* vn2, lapack_complex_float* work);

    void FC_GLOBAL(claqps,CLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, lapack_complex_float* a, blasint* lda, blasint* jpvt, lapack_complex_float* tau, float* vn1, float* vn2, lapack_complex_float* auxv, lapack_complex_float* f, blasint* ldf);

    void FC_GLOBAL(claqr0,CLAQR0)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* h, blasint* ldh, lapack_complex_float* w, blasint* iloz, blasint* ihiz, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(claqr1,CLAQR1)(blasint* n, lapack_complex_float* h, blasint* ldh, lapack_complex_float* s1, lapack_complex_float* s2, lapack_complex_float* v);

    void FC_GLOBAL(claqr2,CLAQR2)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, lapack_complex_float* h, blasint* ldh, blasint* iloz, blasint* ihiz, lapack_complex_float* z, blasint* ldz, blasint* ns, blasint* nd, lapack_complex_float* sh, lapack_complex_float* v, blasint* ldv, blasint* nh, lapack_complex_float* t, blasint* ldt, blasint* nv, lapack_complex_float* wv, blasint* ldwv, lapack_complex_float* work, blasint* lwork);

    void FC_GLOBAL(claqr3,CLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, lapack_complex_float* h, blasint* ldh, blasint* iloz, blasint* ihiz, lapack_complex_float* z, blasint* ldz, blasint* ns, blasint* nd, lapack_complex_float* sh, lapack_complex_float* v, blasint* ldv, blasint* nh, lapack_complex_float* t, blasint* ldt, blasint* nv, lapack_complex_float* wv, blasint* ldwv, lapack_complex_float* work, blasint* lwork);

    void FC_GLOBAL(claqr4,CLAQR4)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* h, blasint* ldh, lapack_complex_float* w, blasint* iloz, blasint* ihiz, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(claqr5,CLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, lapack_complex_float* s, lapack_complex_float* h, blasint* ldh, blasint* iloz, blasint* ihiz, lapack_complex_float* z, blasint* ldz, lapack_complex_float* v, blasint* ldv, lapack_complex_float* u, blasint* ldu, blasint* nv, lapack_complex_float* wv, blasint* ldwv, blasint* nh, lapack_complex_float* wh, blasint* ldwh);

    void FC_GLOBAL(claqsb,CLAQSB)(char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(claqsp,CLAQSP)(char* uplo, blasint* n, lapack_complex_float* ap, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(claqsy,CLAQSY)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(clar1v,CLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, lapack_complex_float* z, blaslogical* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work);

    void FC_GLOBAL(clar2v,CLAR2V)(blasint* n, lapack_complex_float* x, lapack_complex_float* y, lapack_complex_float* z, blasint* incx, float* c, lapack_complex_float* s, blasint* incc);

    void FC_GLOBAL(clarcm,CLARCM)(blasint* m, blasint* n, float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* c, blasint* ldc, float* rwork);

    void FC_GLOBAL(clarf,CLARF)(char* side, blasint* m, blasint* n, lapack_complex_float* v, blasint* incv, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(clarfb,CLARFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, lapack_complex_float* v, blasint* ldv, lapack_complex_float* t, blasint* ldt, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(clarfg,CLARFG)(blasint* n, lapack_complex_float* alpha, lapack_complex_float* x, blasint* incx, lapack_complex_float* tau);

    void FC_GLOBAL(clarfgp,CLARFGP)(blasint* n, lapack_complex_float* alpha, lapack_complex_float* x, blasint* incx, lapack_complex_float* tau);

    void FC_GLOBAL(clarft,CLARFT)(char* direct, char* storev, blasint* n, blasint* k, lapack_complex_float* v, blasint* ldv, lapack_complex_float* tau, lapack_complex_float* t, blasint* ldt, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(clarfx,CLARFX)(char* side, blasint* m, blasint* n, lapack_complex_float* v, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(clargv,CLARGV)(blasint* n, lapack_complex_float* x, blasint* incx, lapack_complex_float* y, blasint* incy, float* c, blasint* incc);

    void FC_GLOBAL(clarnv,CLARNV)(blasint* idist, blasint* iseed, blasint* n, lapack_complex_float* x);

    void FC_GLOBAL(clarrv,CLARRV)(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, lapack_complex_float* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(clartg,CLARTG)(lapack_complex_float* f, lapack_complex_float* g, float* cs, lapack_complex_float* sn, lapack_complex_float* r);

    void FC_GLOBAL(clartv,CLARTV)(blasint* n, lapack_complex_float* x, blasint* incx, lapack_complex_float* y, blasint* incy, float* c, lapack_complex_float* s, blasint* incc);

    void FC_GLOBAL(clarz,CLARZ)(char* side, blasint* m, blasint* n, blasint* l, lapack_complex_float* v, blasint* incv, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(clarzb,CLARZB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, lapack_complex_float* v, blasint* ldv, lapack_complex_float* t, blasint* ldt, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(clarzt,CLARZT)(char* direct, char* storev, blasint* n, blasint* k, lapack_complex_float* v, blasint* ldv, lapack_complex_float* tau, lapack_complex_float* t, blasint* ldt, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(clascl,CLASCL)(char* type_bn, blasint* kl, blasint* ku, float* cfrom, float* cto, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn);

    void FC_GLOBAL(claset,CLASET)(char* uplo, blasint* m, blasint* n, lapack_complex_float* alpha, lapack_complex_float* beta, lapack_complex_float* a, blasint* lda, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(clasr,CLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, float* c, float* s, lapack_complex_float* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct);

    void FC_GLOBAL(classq,CLASSQ)(blasint* n, lapack_complex_float* x, blasint* incx, float* scale, float* sumsq);

    void FC_GLOBAL(claswp,CLASWP)(blasint* n, lapack_complex_float* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx);

    void FC_GLOBAL(clasyf,CLASYF)(char* uplo, blasint* n, blasint* nb, blasint* kb, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(clatbs,CLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* x, float* scale, float* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(clatdf,CLATDF)(blasint* ijob, blasint* n, lapack_complex_float* z, blasint* ldz, lapack_complex_float* rhs, float* rdsum, float* rdscal, blasint* ipiv, blasint* jpiv);

    void FC_GLOBAL(clatps,CLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, lapack_complex_float* ap, lapack_complex_float* x, float* scale, float* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(clatrd,CLATRD)(char* uplo, blasint* n, blasint* nb, lapack_complex_float* a, blasint* lda, float* e, lapack_complex_float* tau, lapack_complex_float* w, blasint* ldw, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(clatrs,CLATRS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* x, float* scale, float* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(clatrz,CLATRZ)(blasint* m, blasint* n, blasint* l, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work);

    void FC_GLOBAL(clatzm,CLATZM)(char* side, blasint* m, blasint* n, lapack_complex_float* v, blasint* incv, lapack_complex_float* tau, lapack_complex_float* c1, lapack_complex_float* c2, blasint* ldc, lapack_complex_float* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(clauu2,CLAUU2)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(clauum,CLAUUM)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpbcon,CPBCON)(char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, float* anorm, float* rcond, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpbequ,CPBEQU)(char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, float* s, float* scond, float* amax, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpbrfs,CPBRFS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* afb, blasint* ldafb, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpbstf,CPBSTF)(char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpbsv,CPBSV)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpbsvx,CPBSVX)(char* fact, char* uplo, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* afb, blasint* ldafb, char* equed, float* s, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(cpbtf2,CPBTF2)(char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpbtrf,CPBTRF)(char* uplo, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpbtrs,CPBTRS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpftrf,CPFTRF)(char* transr, char* uplo, blasint* n, lapack_complex_float* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpftri,CPFTRI)(char* transr, char* uplo, blasint* n, lapack_complex_float* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpftrs,CPFTRS)(char* transr, char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpocon,CPOCON)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* anorm, float* rcond, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpoequ,CPOEQU)(blasint* n, lapack_complex_float* a, blasint* lda, float* s, float* scond, float* amax, blasint* info);

    void FC_GLOBAL(cpoequb,CPOEQUB)(blasint* n, lapack_complex_float* a, blasint* lda, float* s, float* scond, float* amax, blasint* info);

    void FC_GLOBAL(cporfs,CPORFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* af, blasint* ldaf, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cposv,CPOSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cposvx,CPOSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* af, blasint* ldaf, char* equed, float* s, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(cpotf2,CPOTF2)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpotrf,CPOTRF)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpotri,CPOTRI)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpotrs,CPOTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cppcon,CPPCON)(char* uplo, blasint* n, lapack_complex_float* ap, float* anorm, float* rcond, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cppequ,CPPEQU)(char* uplo, blasint* n, lapack_complex_float* ap, float* s, float* scond, float* amax, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpprfs,CPPRFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* afp, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cppsv,CPPSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cppsvx,CPPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* afp, char* equed, float* s, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(cpptrf,CPPTRF)(char* uplo, blasint* n, lapack_complex_float* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpptri,CPPTRI)(char* uplo, blasint* n, lapack_complex_float* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpptrs,CPPTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpstf2,CPSTF2)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* piv, blasint* rank_bn, float* tol, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cpstrf,CPSTRF)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* piv, blasint* rank_bn, float* tol, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cptcon,CPTCON)(blasint* n, float* d, lapack_complex_float* e, float* anorm, float* rcond, float* rwork, blasint* info);

    void FC_GLOBAL(cpteqr,CPTEQR)(char* compz, blasint* n, float* d, float* e, lapack_complex_float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(cptrfs,CPTRFS)(char* uplo, blasint* n, blasint* nrhs, float* d, lapack_complex_float* e, float* df, lapack_complex_float* ef, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cptsv,CPTSV)(blasint* n, blasint* nrhs, float* d, lapack_complex_float* e, lapack_complex_float* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(cptsvx,CPTSVX)(char* fact, blasint* n, blasint* nrhs, float* d, lapack_complex_float* e, float* df, lapack_complex_float* ef, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact);

    void FC_GLOBAL(cpttrf,CPTTRF)(blasint* n, float* d, lapack_complex_float* e, blasint* info);

    void FC_GLOBAL(cpttrs,CPTTRS)(char* uplo, blasint* n, blasint* nrhs, float* d, lapack_complex_float* e, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cptts2,CPTTS2)(blasint* iuplo, blasint* n, blasint* nrhs, float* d, lapack_complex_float* e, lapack_complex_float* b, blasint* ldb);

    void FC_GLOBAL(crot,CROT)(blasint* n, lapack_complex_float* cx, blasint* incx, lapack_complex_float* cy, blasint* incy, float* c, lapack_complex_float* s);

    void FC_GLOBAL(cspcon,CSPCON)(char* uplo, blasint* n, lapack_complex_float* ap, blasint* ipiv, float* anorm, float* rcond, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cspmv,CSPMV)(char* uplo, blasint* n, lapack_complex_float* alpha, lapack_complex_float* ap, lapack_complex_float* x, blasint* incx, lapack_complex_float* beta, lapack_complex_float* y, blasint* incy, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cspr,CSPR)(char* uplo, blasint* n, lapack_complex_float* alpha, lapack_complex_float* x, blasint* incx, lapack_complex_float* ap, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csprfs,CSPRFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* afp, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cspsv,CSPSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cspsvx,CSPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* afp, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csptrf,CSPTRF)(char* uplo, blasint* n, lapack_complex_float* ap, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csptri,CSPTRI)(char* uplo, blasint* n, lapack_complex_float* ap, blasint* ipiv, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csptrs,CSPTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* ap, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csrscl,CSRSCL)(blasint* n, float* sa, lapack_complex_float* sx, blasint* incx);

    void FC_GLOBAL(cstedc,CSTEDC)(char* compz, blasint* n, float* d, float* e, lapack_complex_float* z, blasint* ldz, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(cstegr,CSTEGR)(char* jobz, char* range, blasint* n, float* d, float* e, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, lapack_complex_float* z, blasint* ldz, blasint* isuppz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(cstein,CSTEIN)(blasint* n, float* d, float* e, blasint* m, float* w, blasint* iblock, blasint* isplit, lapack_complex_float* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info);

    void FC_GLOBAL(cstemr,CSTEMR)(char* jobz, char* range, blasint* n, float* d, float* e, float* vl, float* vu, blasint* il, blasint* iu, blasint* m, float* w, lapack_complex_float* z, blasint* ldz, blasint* nzc, blasint* isuppz, blaslogical* tryrac, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(csteqr,CSTEQR)(char* compz, blasint* n, float* d, float* e, lapack_complex_float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(csycon,CSYCON)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, float* anorm, float* rcond, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csyconv,CSYCONV)(char* uplo, char* way, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    void FC_GLOBAL(csyequb,CSYEQUB)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, float* s, float* scond, float* amax, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csymv,CSYMV)(char* uplo, blasint* n, lapack_complex_float* alpha, lapack_complex_float* a, blasint* lda, lapack_complex_float* x, blasint* incx, lapack_complex_float* beta, lapack_complex_float* y, blasint* incy, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csyr,CSYR)(char* uplo, blasint* n, lapack_complex_float* alpha, lapack_complex_float* x, blasint* incx, lapack_complex_float* a, blasint* lda, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csyrfs,CSYRFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* af, blasint* ldaf, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csysv,CSYSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csysvx,CSYSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* af, blasint* ldaf, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* rcond, float* ferr, float* berr, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csyswapr,CSYSWAPR)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* i1, blasint* i2, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csytf2,CSYTF2)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csytrf,CSYTRF)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csytri,CSYTRI)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csytri2,CSYTRI2)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csytri2x,CSYTRI2X)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* work, blasint* nb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csytrs,CSYTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(csytrs2,CSYTRS2)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, blasint* ipiv, lapack_complex_float* b, blasint* ldb, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ctbcon,CTBCON)(char* norm, char* uplo, char* diag, blasint* n, blasint* kd, lapack_complex_float* ab, blasint* ldab, float* rcond, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctbrfs,CTBRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctbtrs,CTBTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_float* ab, blasint* ldab, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctfsm,CTFSM)(char* transr, char* side, char* uplo, char* trans, char* diag, blasint* m, blasint* n, lapack_complex_float* alpha, lapack_complex_float* a, lapack_complex_float* b, blasint* ldb, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctftri,CTFTRI)(char* transr, char* uplo, char* diag, blasint* n, lapack_complex_float* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctfttp,CTFTTP)(char* transr, char* uplo, blasint* n, lapack_complex_float* arf, lapack_complex_float* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ctfttr,CTFTTR)(char* transr, char* uplo, blasint* n, lapack_complex_float* arf, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ctgevc,CTGEVC)(char* side, char* howmny, blaslogical* selectfunc, blasint* n, lapack_complex_float* s, blasint* lds, lapack_complex_float* p, blasint* ldp, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, blasint* mm, blasint* m, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(ctgex2,CTGEX2)(blaslogical* wantq, blaslogical* wantz, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* q, blasint* ldq, lapack_complex_float* z, blasint* ldz, blasint* j1, blasint* info);

    void FC_GLOBAL(ctgexc,CTGEXC)(blaslogical* wantq, blaslogical* wantz, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* q, blasint* ldq, lapack_complex_float* z, blasint* ldz, blasint* ifst, blasint* ilst, blasint* info);

    void FC_GLOBAL(ctgsen,CTGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* selectfunc, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* alpha, lapack_complex_float* beta, lapack_complex_float* q, blasint* ldq, lapack_complex_float* z, blasint* ldz, blasint* m, float* pl, float* pr, float* dif, lapack_complex_float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info);

    void FC_GLOBAL(ctgsja,CTGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, lapack_complex_float* u, blasint* ldu, lapack_complex_float* v, blasint* ldv, lapack_complex_float* q, blasint* ldq, lapack_complex_float* work, blasint* ncycle, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(ctgsna,CTGSNA)(char* job, char* howmny, blaslogical* selectfunc, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, float* s, float* dif, blasint* mm, blasint* m, lapack_complex_float* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(ctgsy2,CTGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* c, blasint* ldc, lapack_complex_float* d, blasint* ldd, lapack_complex_float* e, blasint* lde, lapack_complex_float* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(ctgsyl,CTGSYL)(char* trans, blasint* ijob, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* c, blasint* ldc, lapack_complex_float* d, blasint* ldd, lapack_complex_float* e, blasint* lde, lapack_complex_float* f, blasint* ldf, float* scale, float* dif, lapack_complex_float* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(ctpcon,CTPCON)(char* norm, char* uplo, char* diag, blasint* n, lapack_complex_float* ap, float* rcond, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctpmqrt,CTPMQRT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, blasint* nb, lapack_complex_float* v, blasint* ldv, lapack_complex_float* t, blasint* ldt, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(ctpqrt,CTPQRT)(blasint* m, blasint* n, blasint* l, blasint* nb, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* t, blasint* ldt, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(ctpqrt2,CTPQRT2)(blasint* m, blasint* n, blasint* l, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(ctprfb,CTPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, lapack_complex_float* v, blasint* ldv, lapack_complex_float* t, blasint* ldt, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(ctprfs,CTPRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctptri,CTPTRI)(char* uplo, char* diag, blasint* n, lapack_complex_float* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctptrs,CTPTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, lapack_complex_float* ap, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctpttf,CTPTTF)(char* transr, char* uplo, blasint* n, lapack_complex_float* ap, lapack_complex_float* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ctpttr,CTPTTR)(char* uplo, blasint* n, lapack_complex_float* ap, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ctrcon,CTRCON)(char* norm, char* uplo, char* diag, blasint* n, lapack_complex_float* a, blasint* lda, float* rcond, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctrevc,CTREVC)(char* side, char* howmny, blaslogical* selectfunc, blasint* n, lapack_complex_float* t, blasint* ldt, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, blasint* mm, blasint* m, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(ctrexc,CTREXC)(char* compq, blasint* n, lapack_complex_float* t, blasint* ldt, lapack_complex_float* q, blasint* ldq, blasint* ifst, blasint* ilst, blasint* info, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(ctrrfs,CTRRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* x, blasint* ldx, float* ferr, float* berr, lapack_complex_float* work, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctrsen,CTRSEN)(char* job, char* compq, blaslogical* selectfunc, blasint* n, lapack_complex_float* t, blasint* ldt, lapack_complex_float* q, blasint* ldq, lapack_complex_float* w, blasint* m, float* s, float* sep, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(ctrsna,CTRSNA)(char* job, char* howmny, blaslogical* selectfunc, blasint* n, lapack_complex_float* t, blasint* ldt, lapack_complex_float* vl, blasint* ldvl, lapack_complex_float* vr, blasint* ldvr, float* s, float* sep, blasint* mm, blasint* m, lapack_complex_float* work, blasint* ldwork, float* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(ctrsyl,CTRSYL)(char* trana, char* tranb, blasint* isgn, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, lapack_complex_float* c, blasint* ldc, float* scale, blasint* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb);

    void FC_GLOBAL(ctrti2,CTRTI2)(char* uplo, char* diag, blasint* n, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctrtri,CTRTRI)(char* uplo, char* diag, blasint* n, lapack_complex_float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctrtrs,CTRTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, lapack_complex_float* a, blasint* lda, lapack_complex_float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ctrttf,CTRTTF)(char* transr, char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ctrttp,CTRTTP)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ctzrqf,CTZRQF)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, blasint* info);

    void FC_GLOBAL(ctzrzf,CTZRZF)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cunbdb,CUNBDB)(char* trans, char* signs, blasint* m, blasint* p, blasint* q, lapack_complex_float* x11, blasint* ldx11, lapack_complex_float* x12, blasint* ldx12, lapack_complex_float* x21, blasint* ldx21, lapack_complex_float* x22, blasint* ldx22, float* theta, float* phi, lapack_complex_float* taup1, lapack_complex_float* taup2, lapack_complex_float* tauq1, lapack_complex_float* tauq2, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    void FC_GLOBAL(cuncsd,CUNCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, lapack_complex_float* x11, blasint* ldx11, lapack_complex_float* x12, blasint* ldx12, lapack_complex_float* x21, blasint* ldx21, lapack_complex_float* x22, blasint* ldx22, float* theta, lapack_complex_float* u1, blasint* ldu1, lapack_complex_float* u2, blasint* ldu2, lapack_complex_float* v1t, blasint* ldv1t, lapack_complex_float* v2t, blasint* ldv2t, lapack_complex_float* work, blasint* lwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    void FC_GLOBAL(cung2l,CUNG2L)(blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cung2r,CUNG2R)(blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cungbr,CUNGBR)(char* vect, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect);

    void FC_GLOBAL(cunghr,CUNGHR)(blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cungl2,CUNGL2)(blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cunglq,CUNGLQ)(blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cungql,CUNGQL)(blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cungqr,CUNGQR)(blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cungr2,CUNGR2)(blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* info);

    void FC_GLOBAL(cungrq,CUNGRQ)(blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(cungtr,CUNGTR)(char* uplo, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cunm2l,CUNM2L)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunm2r,CUNM2R)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmbr,CUNMBR)(char* vect, char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmhr,CUNMHR)(char* side, char* trans, blasint* m, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunml2,CUNML2)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmlq,CUNMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmql,CUNMQL)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmqr,CUNMQR)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmr2,CUNMR2)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmr3,CUNMR3)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmrq,CUNMRQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmrz,CUNMRZ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cunmtr,CUNMTR)(char* side, char* uplo, char* trans, blasint* m, blasint* n, lapack_complex_float* a, blasint* lda, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(cupgtr,CUPGTR)(char* uplo, blasint* n, lapack_complex_float* ap, lapack_complex_float* tau, lapack_complex_float* q, blasint* ldq, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(cupmtr,CUPMTR)(char* side, char* uplo, char* trans, blasint* m, blasint* n, lapack_complex_float* ap, lapack_complex_float* tau, lapack_complex_float* c, blasint* ldc, lapack_complex_float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dbbcsd,DBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dbdsdc,DBDSDC)(char* uplo, char* compq, blasint* n, double* d, double* e, double* u, blasint* ldu, double* vt, blasint* ldvt, double* q, blasint* iq, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(dbdsqr,DBDSQR)(char* uplo, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, double* d, double* e, double* vt, blasint* ldvt, double* u, blasint* ldu, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ddisna,DDISNA)(char* job, blasint* m, blasint* n, double* d, double* sep, blasint* info, flexiblas_fortran_charlen_t len_job);

    void FC_GLOBAL(dgbbrd,DGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double* ab, blasint* ldab, double* d, double* e, double* q, blasint* ldq, double* pt, blasint* ldpt, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_vect);

    void FC_GLOBAL(dgbcon,DGBCON)(char* norm, blasint* n, blasint* kl, blasint* ku, double* ab, blasint* ldab, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(dgbequ,DGBEQU)(blasint* m, blasint* n, blasint* kl, blasint* ku, double* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, blasint* info);

    void FC_GLOBAL(dgbequb,DGBEQUB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, blasint* info);

    void FC_GLOBAL(dgbrfs,DGBRFS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, double* ab, blasint* ldab, double* afb, blasint* ldafb, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dgbsv,DGBSV)(blasint* n, blasint* kl, blasint* ku, blasint* nrhs, double* ab, blasint* ldab, blasint* ipiv, double* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(dgbsvx,DGBSVX)(char* fact, char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, double* ab, blasint* ldab, double* afb, blasint* ldafb, blasint* ipiv, char* equed, double* r, double* c, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dgbtf2,DGBTF2)(blasint* m, blasint* n, blasint* kl, blasint* ku, double* ab, blasint* ldab, blasint* ipiv, blasint* info);

    void FC_GLOBAL(dgbtrf,DGBTRF)(blasint* m, blasint* n, blasint* kl, blasint* ku, double* ab, blasint* ldab, blasint* ipiv, blasint* info);

    void FC_GLOBAL(dgbtrs,DGBTRS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, double* ab, blasint* ldab, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dgebak,DGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(dgebal,DGEBAL)(char* job, blasint* n, double* a, blasint* lda, blasint* ilo, blasint* ihi, double* scale, blasint* info, flexiblas_fortran_charlen_t len_job);

    void FC_GLOBAL(dgebd2,DGEBD2)(blasint* m, blasint* n, double* a, blasint* lda, double* d, double* e, double* tauq, double* taup, double* work, blasint* info);

    void FC_GLOBAL(dgebrd,DGEBRD)(blasint* m, blasint* n, double* a, blasint* lda, double* d, double* e, double* tauq, double* taup, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgecon,DGECON)(char* norm, blasint* n, double* a, blasint* lda, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(dgeequ,DGEEQU)(blasint* m, blasint* n, double* a, blasint* lda, double* r, double* c, double* rowcnd, double* colcnd, double* amax, blasint* info);

    void FC_GLOBAL(dgeequb,DGEEQUB)(blasint* m, blasint* n, double* a, blasint* lda, double* r, double* c, double* rowcnd, double* colcnd, double* amax, blasint* info);

    void FC_GLOBAL(dgees,DGEES)(char* jobvs, char* sort, blaslogical* selectfunc, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

    void FC_GLOBAL(dgeesx,DGEESX)(char* jobvs, char* sort, blaslogical* selectfunc, char* sense, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(dgeev,DGEEV)(char* jobvl, char* jobvr, blasint* n, double* a, blasint* lda, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(dgeevx,DGEEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, double* a, blasint* lda, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* ilo, blasint* ihi, double* scale, double* abnrm, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(dgegs,DGEGS)(char* jobvsl, char* jobvsr, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* vsl, blasint* ldvsl, double* vsr, blasint* ldvsr, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    void FC_GLOBAL(dgegv,DGEGV)(char* jobvl, char* jobvr, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* vl, blasint* ldvl, double* vr, blasint* ldvr, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(dgehd2,DGEHD2)(blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dgehrd,DGEHRD)(blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgejsv,DGEJSV)(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, double* a, blasint* lda, double* sva, double* u, blasint* ldu, double* v, blasint* ldv, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobp);

    void FC_GLOBAL(dgelq2,DGELQ2)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dgelqf,DGELQF)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgels,DGELS)(char* trans, blasint* m, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dgelsd,DGELSD)(blasint* m, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* s, double* rcond, blasint* rank_bn, double* work, blasint* lwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(dgelss,DGELSS)(blasint* m, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* s, double* rcond, blasint* rank_bn, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgelsx,DGELSX)(blasint* m, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, blasint* jpvt, double* rcond, blasint* rank_bn, double* work, blasint* info);

    void FC_GLOBAL(dgelsy,DGELSY)(blasint* m, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, blasint* jpvt, double* rcond, blasint* rank_bn, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgemqrt,DGEMQRT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* nb, double* v, blasint* ldv, double* t, blasint* ldt, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dgeql2,DGEQL2)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dgeqlf,DGEQLF)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgeqp3,DGEQP3)(blasint* m, blasint* n, double* a, blasint* lda, blasint* jpvt, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgeqpf,DGEQPF)(blasint* m, blasint* n, double* a, blasint* lda, blasint* jpvt, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dgeqr2,DGEQR2)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dgeqr2p,DGEQR2P)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dgeqrf,DGEQRF)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgeqrfp,DGEQRFP)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgeqrt,DGEQRT)(blasint* m, blasint* n, blasint* nb, double* a, blasint* lda, double* t, blasint* ldt, double* work, blasint* info);

    void FC_GLOBAL(dgeqrt2,DGEQRT2)(blasint* m, blasint* n, double* a, blasint* lda, double* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(dgeqrt3,DGEQRT3)(blasint* m, blasint* n, double* a, blasint* lda, double* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(dgerfs,DGERFS)(char* trans, blasint* n, blasint* nrhs, double* a, blasint* lda, double* af, blasint* ldaf, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dgerq2,DGERQ2)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dgerqf,DGERQF)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgesc2,DGESC2)(blasint* n, double* a, blasint* lda, double* rhs, blasint* ipiv, blasint* jpiv, double* scale);

    void FC_GLOBAL(dgesdd,DGESDD)(char* jobz, blasint* m, blasint* n, double* a, blasint* lda, double* s, double* u, blasint* ldu, double* vt, blasint* ldvt, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz);

    void FC_GLOBAL(dgesv,DGESV)(blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(dgesvd,DGESVD)(char* jobu, char* jobvt, blasint* m, blasint* n, double* a, blasint* lda, double* s, double* u, blasint* ldu, double* vt, blasint* ldvt, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt);

    void FC_GLOBAL(dgesvj,DGESVJ)(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, double* a, blasint* lda, double* sva, blasint* mv, double* v, blasint* ldv, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);

    void FC_GLOBAL(dgesvx,DGESVX)(char* fact, char* trans, blasint* n, blasint* nrhs, double* a, blasint* lda, double* af, blasint* ldaf, blasint* ipiv, char* equed, double* r, double* c, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dgetc2,DGETC2)(blasint* n, double* a, blasint* lda, blasint* ipiv, blasint* jpiv, blasint* info);

    void FC_GLOBAL(dgetf2,DGETF2)(blasint* m, blasint* n, double* a, blasint* lda, blasint* ipiv, blasint* info);

    void FC_GLOBAL(dgetrf,DGETRF)(blasint* m, blasint* n, double* a, blasint* lda, blasint* ipiv, blasint* info);

    void FC_GLOBAL(dgetri,DGETRI)(blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgetrs,DGETRS)(char* trans, blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dggbak,DGGBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* lscale, double* rscale, blasint* m, double* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(dggbal,DGGBAL)(char* job, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* work, blasint* info, flexiblas_fortran_charlen_t len_job);

    void FC_GLOBAL(dgges,DGGES)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* sdim, double* alphar, double* alphai, double* beta, double* vsl, blasint* ldvsl, double* vsr, blasint* ldvsr, double* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort);

    void FC_GLOBAL(dggesx,DGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* sdim, double* alphar, double* alphai, double* beta, double* vsl, blasint* ldvsl, double* vsr, blasint* ldvsr, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(dggev,DGGEV)(char* jobvl, char* jobvr, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* vl, blasint* ldvl, double* vr, blasint* ldvr, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(dggevx,DGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* abnrm, double* bbnrm, double* rconde, double* rcondv, double* work, blasint* lwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(dggglm,DGGGLM)(blasint* n, blasint* m, blasint* p, double* a, blasint* lda, double* b, blasint* ldb, double* d, double* x, double* y, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dgghrd,DGGHRD)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, blasint* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(dgglse,DGGLSE)(blasint* m, blasint* n, blasint* p, double* a, blasint* lda, double* b, blasint* ldb, double* c, double* d, double* x, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dggqrf,DGGQRF)(blasint* n, blasint* m, blasint* p, double* a, blasint* lda, double* taua, double* b, blasint* ldb, double* taub, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dggrqf,DGGRQF)(blasint* m, blasint* p, blasint* n, double* a, blasint* lda, double* taua, double* b, blasint* ldb, double* taub, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dggsvd,DGGSVD)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* n, blasint* p, blasint* k, blasint* l, double* a, blasint* lda, double* b, blasint* ldb, double* alpha, double* beta, double* u, blasint* ldu, double* v, blasint* ldv, double* q, blasint* ldq, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(dggsvp,DGGSVP)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double* u, blasint* ldu, double* v, blasint* ldv, double* q, blasint* ldq, blasint* iwork, double* tau, double* work, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(dgsvj0,DGSVJ0)(char* jobv, blasint* m, blasint* n, double* a, blasint* lda, double* d, double* sva, blasint* mv, double* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobv);

    void FC_GLOBAL(dgsvj1,DGSVJ1)(char* jobv, blasint* m, blasint* n, blasint* n1, double* a, blasint* lda, double* d, double* sva, blasint* mv, double* v, blasint* ldv, double* eps, double* sfmin, double* tol, blasint* nsweep, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobv);

    void FC_GLOBAL(dgtcon,DGTCON)(char* norm, blasint* n, double* dl, double* d, double* du, double* du2, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(dgtrfs,DGTRFS)(char* trans, blasint* n, blasint* nrhs, double* dl, double* d, double* du, double* dlf, double* df, double* duf, double* du2, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dgtsv,DGTSV)(blasint* n, blasint* nrhs, double* dl, double* d, double* du, double* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(dgtsvx,DGTSVX)(char* fact, char* trans, blasint* n, blasint* nrhs, double* dl, double* d, double* du, double* dlf, double* df, double* duf, double* du2, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dgttrf,DGTTRF)(blasint* n, double* dl, double* d, double* du, double* du2, blasint* ipiv, blasint* info);

    void FC_GLOBAL(dgttrs,DGTTRS)(char* trans, blasint* n, blasint* nrhs, double* dl, double* d, double* du, double* du2, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dgtts2,DGTTS2)(blasint* itrans, blasint* n, blasint* nrhs, double* dl, double* d, double* du, double* du2, blasint* ipiv, double* b, blasint* ldb);

    void FC_GLOBAL(dhgeqz,DHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, double* h, blasint* ldh, double* t, blasint* ldt, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(dhsein,DHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* selectfunc, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

    void FC_GLOBAL(dhseqr,DHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, double* h, blasint* ldh, double* wr, double* wi, double* z, blasint* ldz, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz);

    blaslogical FC_GLOBAL(disnan,DISNAN)(double* din);

    void FC_GLOBAL(dlabad,DLABAD)(double* small, double* large);

    void FC_GLOBAL(dlabrd,DLABRD)(blasint* m, blasint* n, blasint* nb, double* a, blasint* lda, double* d, double* e, double* tauq, double* taup, double* x, blasint* ldx, double* y, blasint* ldy);

    void FC_GLOBAL(dlacn2,DLACN2)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase, blasint* isave);

    void FC_GLOBAL(dlacon,DLACON)(blasint* n, double* v, double* x, blasint* isgn, double* est, blasint* kase);

    void FC_GLOBAL(dlacpy,DLACPY)(char* uplo, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dladiv,DLADIV)(double* a, double* b, double* c, double* d, double* p, double* q);

    void FC_GLOBAL(dlae2,DLAE2)(double* a, double* b, double* c, double* rt1, double* rt2);

    void FC_GLOBAL(dlaebz,DLAEBZ)(blasint* ijob, blasint* nitmax, blasint* n, blasint* mmax, blasint* minp, blasint* nbmin, double* abstol, double* reltol, double* pivmin, double* d, double* e, double* e2, blasint* nval, double* ab, double* c, blasint* mout, blasint* nab, double* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(dlaed0,DLAED0)(blasint* icompq, blasint* qsiz, blasint* n, double* d, double* e, double* q, blasint* ldq, double* qstore, blasint* ldqs, double* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(dlaed1,DLAED1)(blasint* n, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, blasint* cutpnt, double* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(dlaed2,DLAED2)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, double* z, double* dlamda, double* w, double* q2, blasint* indx, blasint* indxc, blasint* indxp, blasint* coltyp, blasint* info);

    void FC_GLOBAL(dlaed3,DLAED3)(blasint* k, blasint* n, blasint* n1, double* d, double* q, blasint* ldq, double* rho, double* dlamda, double* q2, blasint* indx, blasint* ctot, double* w, double* s, blasint* info);

    void FC_GLOBAL(dlaed4,DLAED4)(blasint* n, blasint* i, double* d, double* z, double* delta, double* rho, double* dlam, blasint* info);

    void FC_GLOBAL(dlaed5,DLAED5)(blasint* i, double* d, double* z, double* delta, double* rho, double* dlam);

    void FC_GLOBAL(dlaed6,DLAED6)(blasint* kniter, blaslogical* orgati, double* rho, double* d, double* z, double* finit, double* tau, blasint* info);

    void FC_GLOBAL(dlaed7,DLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, blasint* cutpnt, double* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, double* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(dlaed8,DLAED8)(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, double* d, double* q, blasint* ldq, blasint* indxq, double* rho, blasint* cutpnt, double* z, double* dlamda, double* q2, blasint* ldq2, double* w, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* indxp, blasint* indx, blasint* info);

    void FC_GLOBAL(dlaed9,DLAED9)(blasint* k, blasint* kstart, blasint* kstop, blasint* n, double* d, double* q, blasint* ldq, double* rho, double* dlamda, double* w, double* s, blasint* lds, blasint* info);

    void FC_GLOBAL(dlaeda,DLAEDA)(blasint* n, blasint* tlvls, blasint* curlvl, blasint* curpbm, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, double* q, blasint* qptr, double* z, double* ztemp, blasint* info);

    void FC_GLOBAL(dlaein,DLAEIN)(blaslogical* rightv, blaslogical* noinit, blasint* n, double* h, blasint* ldh, double* wr, double* wi, double* vr, double* vi, double* b, blasint* ldb, double* work, double* eps3, double* smlnum, double* bignum, blasint* info);

    void FC_GLOBAL(dlaev2,DLAEV2)(double* a, double* b, double* c, double* rt1, double* rt2, double* cs1, double* sn1);

    void FC_GLOBAL(dlaexc,DLAEXC)(blaslogical* wantq, blasint* n, double* t, blasint* ldt, double* q, blasint* ldq, blasint* j1, blasint* n1, blasint* n2, double* work, blasint* info);

    void FC_GLOBAL(dlag2,DLAG2)(double* a, blasint* lda, double* b, blasint* ldb, double* safmin, double* scale1, double* scale2, double* wr1, double* wr2, double* wi);

    void FC_GLOBAL(dlag2s,DLAG2S)(blasint* m, blasint* n, double* a, blasint* lda, float* sa, blasint* ldsa, blasint* info);

    void FC_GLOBAL(dlags2,DLAGS2)(blaslogical* upper, double* a1, double* a2, double* a3, double* b1, double* b2, double* b3, double* csu, double* snu, double* csv, double* snv, double* csq, double* snq);

    void FC_GLOBAL(dlagtf,DLAGTF)(blasint* n, double* a, double* lambda, double* b, double* c, double* tol, double* d, blasint* in, blasint* info);

    void FC_GLOBAL(dlagtm,DLAGTM)(char* trans, blasint* n, blasint* nrhs, double* alpha, double* dl, double* d, double* du, double* x, blasint* ldx, double* beta, double* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dlagts,DLAGTS)(blasint* job, blasint* n, double* a, double* b, double* c, double* d, blasint* in, double* y, double* tol, blasint* info);

    void FC_GLOBAL(dlagv2,DLAGV2)(double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* csl, double* snl, double* csr, double* snr);

    void FC_GLOBAL(dlahqr,DLAHQR)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, double* h, blasint* ldh, double* wr, double* wi, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* info);

    void FC_GLOBAL(dlahr2,DLAHR2)(blasint* n, blasint* k, blasint* nb, double* a, blasint* lda, double* tau, double* t, blasint* ldt, double* y, blasint* ldy);

    void FC_GLOBAL(dlahrd,DLAHRD)(blasint* n, blasint* k, blasint* nb, double* a, blasint* lda, double* tau, double* t, blasint* ldt, double* y, blasint* ldy);

    void FC_GLOBAL(dlaic1,DLAIC1)(blasint* job, blasint* j, double* x, double* sest, double* w, double* gamma, double* sestpr, double* s, double* c);

    blaslogical FC_GLOBAL(dlaisnan,DLAISNAN)(double* din1, double* din2);

    void FC_GLOBAL(dlaln2,DLALN2)(blaslogical* ltrans, blasint* na, blasint* nw, double* smin, double* ca, double* a, blasint* lda, double* d1, double* d2, double* b, blasint* ldb, double* wr, double* wi, double* x, blasint* ldx, double* scale, double* xnorm, blasint* info);

    void FC_GLOBAL(dlals0,DLALS0)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, double* b, blasint* ldb, double* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* work, blasint* info);

    void FC_GLOBAL(dlalsa,DLALSA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* nrhs, double* b, blasint* ldb, double* bx, blasint* ldbx, double* u, blasint* ldu, double* vt, blasint* k, double* difl, double* difr, double* z, double* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, double* givnum, double* c, double* s, double* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(dlalsd,DLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb, double* rcond, blasint* rank_bn, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(dlamc3,DLAMC3)(double* a, double* b);

    double FC_GLOBAL(dlamch,DLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach);

    void FC_GLOBAL(dlamrg,DLAMRG)(blasint* n1, blasint* n2, double* a, blasint* dtrd1, blasint* dtrd2, blasint* index_bn);

    blasint FC_GLOBAL(dlaneg,DLANEG)(blasint* n, double* d, double* lld, double* sigma, double* pivmin, blasint* r);

    double FC_GLOBAL(dlangb,DLANGB)(char* norm, blasint* n, blasint* kl, blasint* ku, double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(dlange,DLANGE)(char* norm, blasint* m, blasint* n, double* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(dlangt,DLANGT)(char* norm, blasint* n, double* dl, double* d, double* du, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(dlanhs,DLANHS)(char* norm, blasint* n, double* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(dlansb,DLANSB)(char* norm, char* uplo, blasint* n, blasint* k, double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(dlansf,DLANSF)(char* norm, char* transr, char* uplo, blasint* n, double* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(dlansp,DLANSP)(char* norm, char* uplo, blasint* n, double* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(dlanst,DLANST)(char* norm, blasint* n, double* d, double* e, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(dlansy,DLANSY)(char* norm, char* uplo, blasint* n, double* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(dlantb,DLANTB)(char* norm, char* uplo, char* diag, blasint* n, blasint* k, double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    double FC_GLOBAL(dlantp,DLANTP)(char* norm, char* uplo, char* diag, blasint* n, double* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    double FC_GLOBAL(dlantr,DLANTR)(char* norm, char* uplo, char* diag, blasint* m, blasint* n, double* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dlanv2,DLANV2)(double* a, double* b, double* c, double* d, double* rt1r, double* rt1i, double* rt2r, double* rt2i, double* cs, double* sn);

    void FC_GLOBAL(dlapll,DLAPLL)(blasint* n, double* x, blasint* incx, double* y, blasint* incy, double* ssmin);

    void FC_GLOBAL(dlapmr,DLAPMR)(blaslogical* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k);

    void FC_GLOBAL(dlapmt,DLAPMT)(blaslogical* forwrd, blasint* m, blasint* n, double* x, blasint* ldx, blasint* k);

    double FC_GLOBAL(dlapy2,DLAPY2)(double* x, double* y);

    double FC_GLOBAL(dlapy3,DLAPY3)(double* x, double* y, double* z);

    void FC_GLOBAL(dlaqgb,DLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, double* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dlaqge,DLAQGE)(blasint* m, blasint* n, double* a, blasint* lda, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dlaqp2,DLAQP2)(blasint* m, blasint* n, blasint* offset, double* a, blasint* lda, blasint* jpvt, double* tau, double* vn1, double* vn2, double* work);

    void FC_GLOBAL(dlaqps,DLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, double* a, blasint* lda, blasint* jpvt, double* tau, double* vn1, double* vn2, double* auxv, double* f, blasint* ldf);

    void FC_GLOBAL(dlaqr0,DLAQR0)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, double* h, blasint* ldh, double* wr, double* wi, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dlaqr1,DLAQR1)(blasint* n, double* h, blasint* ldh, double* sr1, double* si1, double* sr2, double* si2, double* v);

    void FC_GLOBAL(dlaqr2,DLAQR2)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, double* h, blasint* ldh, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* ns, blasint* nd, double* sr, double* si, double* v, blasint* ldv, blasint* nh, double* t, blasint* ldt, blasint* nv, double* wv, blasint* ldwv, double* work, blasint* lwork);

    void FC_GLOBAL(dlaqr3,DLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, double* h, blasint* ldh, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* ns, blasint* nd, double* sr, double* si, double* v, blasint* ldv, blasint* nh, double* t, blasint* ldt, blasint* nv, double* wv, blasint* ldwv, double* work, blasint* lwork);

    void FC_GLOBAL(dlaqr4,DLAQR4)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, double* h, blasint* ldh, double* wr, double* wi, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dlaqr5,DLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, double* sr, double* si, double* h, blasint* ldh, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, double* v, blasint* ldv, double* u, blasint* ldu, blasint* nv, double* wv, blasint* ldwv, blasint* nh, double* wh, blasint* ldwh);

    void FC_GLOBAL(dlaqsb,DLAQSB)(char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, double* s, double* scond, double* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dlaqsp,DLAQSP)(char* uplo, blasint* n, double* ap, double* s, double* scond, double* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dlaqsy,DLAQSY)(char* uplo, blasint* n, double* a, blasint* lda, double* s, double* scond, double* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dlaqtr,DLAQTR)(blaslogical* ltran, blaslogical* lreal, blasint* n, double* t, blasint* ldt, double* b, double* w, double* scale, double* x, double* work, blasint* info);

    void FC_GLOBAL(dlar1v,DLAR1V)(blasint* n, blasint* b1, blasint* bn, double* lambda, double* d, double* l, double* ld, double* lld, double* pivmin, double* gaptol, double* z, blaslogical* wantnc, blasint* negcnt, double* ztz, double* mingma, blasint* r, blasint* isuppz, double* nrminv, double* resid, double* rqcorr, double* work);

    void FC_GLOBAL(dlar2v,DLAR2V)(blasint* n, double* x, double* y, double* z, blasint* incx, double* c, double* s, blasint* incc);

    void FC_GLOBAL(dlarf,DLARF)(char* side, blasint* m, blasint* n, double* v, blasint* incv, double* tau, double* c, blasint* ldc, double* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(dlarfb,DLARFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, double* v, blasint* ldv, double* t, blasint* ldt, double* c, blasint* ldc, double* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(dlarfg,DLARFG)(blasint* n, double* alpha, double* x, blasint* incx, double* tau);

    void FC_GLOBAL(dlarfgp,DLARFGP)(blasint* n, double* alpha, double* x, blasint* incx, double* tau);

    void FC_GLOBAL(dlarft,DLARFT)(char* direct, char* storev, blasint* n, blasint* k, double* v, blasint* ldv, double* tau, double* t, blasint* ldt, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(dlarfx,DLARFX)(char* side, blasint* m, blasint* n, double* v, double* tau, double* c, blasint* ldc, double* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(dlargv,DLARGV)(blasint* n, double* x, blasint* incx, double* y, blasint* incy, double* c, blasint* incc);

    void FC_GLOBAL(dlarnv,DLARNV)(blasint* idist, blasint* iseed, blasint* n, double* x);

    void FC_GLOBAL(dlarra,DLARRA)(blasint* n, double* d, double* e, double* e2, double* spltol, double* tnrm, blasint* nsplit, blasint* isplit, blasint* info);

    void FC_GLOBAL(dlarrb,DLARRB)(blasint* n, double* d, double* lld, blasint* ifirst, blasint* ilast, double* rtol1, double* rtol2, blasint* offset, double* w, double* wgap, double* werr, double* work, blasint* iwork, double* pivmin, double* spdiam, blasint* twist, blasint* info);

    void FC_GLOBAL(dlarrc,DLARRC)(char* jobt, blasint* n, double* vl, double* vu, double* d, double* e, double* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt);

    void FC_GLOBAL(dlarrd,DLARRD)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* gers, double* reltol, double* d, double* e, double* e2, double* pivmin, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wl, double* wu, blasint* iblock, blasint* indexw, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

    void FC_GLOBAL(dlarre,DLARRE)(char* range, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* d, double* e, double* e2, double* rtol1, double* rtol2, double* spltol, blasint* nsplit, blasint* isplit, blasint* m, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* pivmin, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(dlarrf,DLARRF)(blasint* n, double* d, double* l, double* ld, blasint* clstrt, blasint* clend, double* w, double* wgap, double* werr, double* spdiam, double* clgapl, double* clgapr, double* pivmin, double* sigma, double* dplus, double* lplus, double* work, blasint* info);

    void FC_GLOBAL(dlarrj,DLARRJ)(blasint* n, double* d, double* e2, blasint* ifirst, blasint* ilast, double* rtol, blasint* offset, double* w, double* werr, double* work, blasint* iwork, double* pivmin, double* spdiam, blasint* info);

    void FC_GLOBAL(dlarrk,DLARRK)(blasint* n, blasint* iw, double* gl, double* gu, double* d, double* e2, double* pivmin, double* reltol, double* w, double* werr, blasint* info);

    void FC_GLOBAL(dlarrr,DLARRR)(blasint* n, double* d, double* e, blasint* info);

    void FC_GLOBAL(dlarrv,DLARRV)(blasint* n, double* vl, double* vu, double* d, double* l, double* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, double* minrgp, double* rtol1, double* rtol2, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(dlartg,DLARTG)(double* f, double* g, double* cs, double* sn, double* r);

    void FC_GLOBAL(dlartgp,DLARTGP)(double* f, double* g, double* cs, double* sn, double* r);

    void FC_GLOBAL(dlartgs,DLARTGS)(double* x, double* y, double* sigma, double* cs, double* sn);

    void FC_GLOBAL(dlartv,DLARTV)(blasint* n, double* x, blasint* incx, double* y, blasint* incy, double* c, double* s, blasint* incc);

    void FC_GLOBAL(dlaruv,DLARUV)(blasint* iseed, blasint* n, double* x);

    void FC_GLOBAL(dlarz,DLARZ)(char* side, blasint* m, blasint* n, blasint* l, double* v, blasint* incv, double* tau, double* c, blasint* ldc, double* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(dlarzb,DLARZB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double* v, blasint* ldv, double* t, blasint* ldt, double* c, blasint* ldc, double* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(dlarzt,DLARZT)(char* direct, char* storev, blasint* n, blasint* k, double* v, blasint* ldv, double* tau, double* t, blasint* ldt, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(dlas2,DLAS2)(double* f, double* g, double* h, double* ssmin, double* ssmax);

    void FC_GLOBAL(dlascl,DLASCL)(char* type_bn, blasint* kl, blasint* ku, double* cfrom, double* cto, blasint* m, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn);

    void FC_GLOBAL(dlasd0,DLASD0)(blasint* n, blasint* sqre, double* d, double* e, double* u, blasint* ldu, double* vt, blasint* ldvt, blasint* smlsiz, blasint* iwork, double* work, blasint* info);

    void FC_GLOBAL(dlasd1,DLASD1)(blasint* nl, blasint* nr, blasint* sqre, double* d, double* alpha, double* beta, double* u, blasint* ldu, double* vt, blasint* ldvt, blasint* idxq, blasint* iwork, double* work, blasint* info);

    void FC_GLOBAL(dlasd2,DLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* alpha, double* beta, double* u, blasint* ldu, double* vt, blasint* ldvt, double* dsigma, double* u2, blasint* ldu2, double* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info);

    void FC_GLOBAL(dlasd3,DLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* q, blasint* ldq, double* dsigma, double* u, blasint* ldu, double* u2, blasint* ldu2, double* vt, blasint* ldvt, double* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, double* z, blasint* info);

    void FC_GLOBAL(dlasd4,DLASD4)(blasint* n, blasint* i, double* d, double* z, double* delta, double* rho, double* sigma, double* work, blasint* info);

    void FC_GLOBAL(dlasd5,DLASD5)(blasint* i, double* d, double* z, double* delta, double* rho, double* dsigma, double* work);

    void FC_GLOBAL(dlasd6,DLASD6)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, double* d, double* vf, double* vl, double* alpha, double* beta, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(dlasd7,DLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, double* d, double* z, double* zw, double* vf, double* vfw, double* vl, double* vlw, double* alpha, double* beta, double* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* c, double* s, blasint* info);

    void FC_GLOBAL(dlasd8,DLASD8)(blasint* icompq, blasint* k, double* d, double* z, double* vf, double* vl, double* difl, double* difr, blasint* lddifr, double* dsigma, double* work, blasint* info);

    void FC_GLOBAL(dlasda,DLASDA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* sqre, double* d, double* e, double* u, blasint* ldu, double* vt, blasint* k, double* difl, double* difr, double* z, double* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, double* givnum, double* c, double* s, double* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(dlasdq,DLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, double* d, double* e, double* vt, blasint* ldvt, double* u, blasint* ldu, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dlasdt,DLASDT)(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub);

    void FC_GLOBAL(dlaset,DLASET)(char* uplo, blasint* m, blasint* n, double* alpha, double* beta, double* a, blasint* lda, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dlasq1,DLASQ1)(blasint* n, double* d, double* e, double* work, blasint* info);

    void FC_GLOBAL(dlasq2,DLASQ2)(blasint* n, double* z, blasint* info);

    void FC_GLOBAL(dlasq3,DLASQ3)(blasint* i0, blasint* n0, double* z, blasint* pp, double* dmin, double* sigma, double* desig, double* qmax, blasint* nfail, blasint* iter, blasint* ndiv, blaslogical* ieee, blasint* ttype, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* g, double* tau);

    void FC_GLOBAL(dlasq4,DLASQ4)(blasint* i0, blasint* n0, double* z, blasint* pp, blasint* n0in, double* dmin, double* dmin1, double* dmin2, double* dn, double* dn1, double* dn2, double* tau, blasint* ttype, double* g);

    void FC_GLOBAL(dlasq5,DLASQ5)(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blaslogical* ieee, double* eps);

    void FC_GLOBAL(dlasq6,DLASQ6)(blasint* i0, blasint* n0, double* z, blasint* pp, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2);

    void FC_GLOBAL(dlasr,DLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, double* c, double* s, double* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct);

    void FC_GLOBAL(dlasrt,DLASRT)(char* id, blasint* n, double* d, blasint* info, flexiblas_fortran_charlen_t len_id);

    void FC_GLOBAL(dlassq,DLASSQ)(blasint* n, double* x, blasint* incx, double* scale, double* sumsq);

    void FC_GLOBAL(dlasv2,DLASV2)(double* f, double* g, double* h, double* ssmin, double* ssmax, double* snr, double* csr, double* snl, double* csl);

    void FC_GLOBAL(dlaswp,DLASWP)(blasint* n, double* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx);

    void FC_GLOBAL(dlasy2,DLASY2)(blaslogical* ltranl, blaslogical* ltranr, blasint* isgn, blasint* n1, blasint* n2, double* tl, blasint* ldtl, double* tr, blasint* ldtr, double* b, blasint* ldb, double* scale, double* x, blasint* ldx, double* xnorm, blasint* info);

    void FC_GLOBAL(dlasyf,DLASYF)(char* uplo, blasint* n, blasint* nb, blasint* kb, double* a, blasint* lda, blasint* ipiv, double* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dlat2s,DLAT2S)(char* uplo, blasint* n, double* a, blasint* lda, float* sa, blasint* ldsa, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dlatbs,DLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, double* ab, blasint* ldab, double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(dlatdf,DLATDF)(blasint* ijob, blasint* n, double* z, blasint* ldz, double* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv);

    void FC_GLOBAL(dlatps,DLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double* ap, double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(dlatrd,DLATRD)(char* uplo, blasint* n, blasint* nb, double* a, blasint* lda, double* e, double* tau, double* w, blasint* ldw, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dlatrs,DLATRS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double* a, blasint* lda, double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(dlatrz,DLATRZ)(blasint* m, blasint* n, blasint* l, double* a, blasint* lda, double* tau, double* work);

    void FC_GLOBAL(dlatzm,DLATZM)(char* side, blasint* m, blasint* n, double* v, blasint* incv, double* tau, double* c1, double* c2, blasint* ldc, double* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(dlauu2,DLAUU2)(char* uplo, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dlauum,DLAUUM)(char* uplo, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dopgtr,DOPGTR)(char* uplo, blasint* n, double* ap, double* tau, double* q, blasint* ldq, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dopmtr,DOPMTR)(char* side, char* uplo, char* trans, blasint* m, blasint* n, double* ap, double* tau, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dorbdb,DORBDB)(char* trans, char* signs, blasint* m, blasint* p, blasint* q, double* x11, blasint* ldx11, double* x12, blasint* ldx12, double* x21, blasint* ldx21, double* x22, blasint* ldx22, double* theta, double* phi, double* taup1, double* taup2, double* tauq1, double* tauq2, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    void FC_GLOBAL(dorcsd,DORCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, double* x11, blasint* ldx11, double* x12, blasint* ldx12, double* x21, blasint* ldx21, double* x22, blasint* ldx22, double* theta, double* u1, blasint* ldu1, double* u2, blasint* ldu2, double* v1t, blasint* ldv1t, double* v2t, blasint* ldv2t, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    void FC_GLOBAL(dorg2l,DORG2L)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dorg2r,DORG2R)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dorgbr,DORGBR)(char* vect, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect);

    void FC_GLOBAL(dorghr,DORGHR)(blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dorgl2,DORGL2)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dorglq,DORGLQ)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dorgql,DORGQL)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dorgqr,DORGQR)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dorgr2,DORGR2)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* info);

    void FC_GLOBAL(dorgrq,DORGRQ)(blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dorgtr,DORGTR)(char* uplo, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dorm2l,DORM2L)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dorm2r,DORM2R)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormbr,DORMBR)(char* vect, char* side, char* trans, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormhr,DORMHR)(char* side, char* trans, blasint* m, blasint* n, blasint* ilo, blasint* ihi, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dorml2,DORML2)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormlq,DORMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormql,DORMQL)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormqr,DORMQR)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormr2,DORMR2)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormr3,DORMR3)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormrq,DORMRQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormrz,DORMRZ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dormtr,DORMTR)(char* side, char* uplo, char* trans, blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* c, blasint* ldc, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dpbcon,DPBCON)(char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpbequ,DPBEQU)(char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, double* s, double* scond, double* amax, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpbrfs,DPBRFS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* afb, blasint* ldafb, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpbstf,DPBSTF)(char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpbsv,DPBSV)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpbsvx,DPBSVX)(char* fact, char* uplo, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* afb, blasint* ldafb, char* equed, double* s, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dpbtf2,DPBTF2)(char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpbtrf,DPBTRF)(char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpbtrs,DPBTRS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpftrf,DPFTRF)(char* transr, char* uplo, blasint* n, double* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpftri,DPFTRI)(char* transr, char* uplo, blasint* n, double* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpftrs,DPFTRS)(char* transr, char* uplo, blasint* n, blasint* nrhs, double* a, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpocon,DPOCON)(char* uplo, blasint* n, double* a, blasint* lda, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpoequ,DPOEQU)(blasint* n, double* a, blasint* lda, double* s, double* scond, double* amax, blasint* info);

    void FC_GLOBAL(dpoequb,DPOEQUB)(blasint* n, double* a, blasint* lda, double* s, double* scond, double* amax, blasint* info);

    void FC_GLOBAL(dporfs,DPORFS)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* af, blasint* ldaf, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dposv,DPOSV)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dposvx,DPOSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* af, blasint* ldaf, char* equed, double* s, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dpotf2,DPOTF2)(char* uplo, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpotrf,DPOTRF)(char* uplo, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpotri,DPOTRI)(char* uplo, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpotrs,DPOTRS)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dppcon,DPPCON)(char* uplo, blasint* n, double* ap, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dppequ,DPPEQU)(char* uplo, blasint* n, double* ap, double* s, double* scond, double* amax, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpprfs,DPPRFS)(char* uplo, blasint* n, blasint* nrhs, double* ap, double* afp, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dppsv,DPPSV)(char* uplo, blasint* n, blasint* nrhs, double* ap, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dppsvx,DPPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, double* ap, double* afp, char* equed, double* s, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(dpptrf,DPPTRF)(char* uplo, blasint* n, double* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpptri,DPPTRI)(char* uplo, blasint* n, double* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpptrs,DPPTRS)(char* uplo, blasint* n, blasint* nrhs, double* ap, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpstf2,DPSTF2)(char* uplo, blasint* n, double* a, blasint* lda, blasint* piv, blasint* rank_bn, double* tol, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dpstrf,DPSTRF)(char* uplo, blasint* n, double* a, blasint* lda, blasint* piv, blasint* rank_bn, double* tol, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dptcon,DPTCON)(blasint* n, double* d, double* e, double* anorm, double* rcond, double* work, blasint* info);

    void FC_GLOBAL(dpteqr,DPTEQR)(char* compz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(dptrfs,DPTRFS)(blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* info);

    void FC_GLOBAL(dptsv,DPTSV)(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(dptsvx,DPTSVX)(char* fact, blasint* n, blasint* nrhs, double* d, double* e, double* df, double* ef, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* info, flexiblas_fortran_charlen_t len_fact);

    void FC_GLOBAL(dpttrf,DPTTRF)(blasint* n, double* d, double* e, blasint* info);

    void FC_GLOBAL(dpttrs,DPTTRS)(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(dptts2,DPTTS2)(blasint* n, blasint* nrhs, double* d, double* e, double* b, blasint* ldb);

    void FC_GLOBAL(drscl,DRSCL)(blasint* n, double* sa, double* sx, blasint* incx);

    void FC_GLOBAL(dsbev,DSBEV)(char* jobz, char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, double* w, double* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsbevd,DSBEVD)(char* jobz, char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsbevx,DSBEVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, double* q, blasint* ldq, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsbgst,DSBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, double* ab, blasint* ldab, double* bb, blasint* ldbb, double* x, blasint* ldx, double* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsbgv,DSBGV)(char* jobz, char* uplo, blasint* n, blasint* ka, blasint* kb, double* ab, blasint* ldab, double* bb, blasint* ldbb, double* w, double* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsbgvd,DSBGVD)(char* jobz, char* uplo, blasint* n, blasint* ka, blasint* kb, double* ab, blasint* ldab, double* bb, blasint* ldbb, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsbgvx,DSBGVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* ka, blasint* kb, double* ab, blasint* ldab, double* bb, blasint* ldbb, double* q, blasint* ldq, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsbtrd,DSBTRD)(char* vect, char* uplo, blasint* n, blasint* kd, double* ab, blasint* ldab, double* d, double* e, double* q, blasint* ldq, double* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(dsecnd,DSECND)(void);

    void FC_GLOBAL(dsfrk,DSFRK)(char* transr, char* uplo, char* trans, blasint* n, blasint* k, double* alpha, double* a, blasint* lda, double* beta, double* c, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dsgesv,DSGESV)(blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info);

    void FC_GLOBAL(dspcon,DSPCON)(char* uplo, blasint* n, double* ap, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dspev,DSPEV)(char* jobz, char* uplo, blasint* n, double* ap, double* w, double* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dspevd,DSPEVD)(char* jobz, char* uplo, blasint* n, double* ap, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dspevx,DSPEVX)(char* jobz, char* range, char* uplo, blasint* n, double* ap, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dspgst,DSPGST)(blasint* itype, char* uplo, blasint* n, double* ap, double* bp, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dspgv,DSPGV)(blasint* itype, char* jobz, char* uplo, blasint* n, double* ap, double* bp, double* w, double* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dspgvd,DSPGVD)(blasint* itype, char* jobz, char* uplo, blasint* n, double* ap, double* bp, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dspgvx,DSPGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, double* ap, double* bp, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsposv,DSPOSV)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* x, blasint* ldx, double* work, float* swork, blasint* iter, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsprfs,DSPRFS)(char* uplo, blasint* n, blasint* nrhs, double* ap, double* afp, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dspsv,DSPSV)(char* uplo, blasint* n, blasint* nrhs, double* ap, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dspsvx,DSPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, double* ap, double* afp, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsptrd,DSPTRD)(char* uplo, blasint* n, double* ap, double* d, double* e, double* tau, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsptrf,DSPTRF)(char* uplo, blasint* n, double* ap, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsptri,DSPTRI)(char* uplo, blasint* n, double* ap, blasint* ipiv, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsptrs,DSPTRS)(char* uplo, blasint* n, blasint* nrhs, double* ap, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dstebz,DSTEBZ)(char* range, char* order, blasint* n, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, double* d, double* e, blasint* m, blasint* nsplit, double* w, blasint* iblock, blasint* isplit, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

    void FC_GLOBAL(dstedc,DSTEDC)(char* compz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(dstegr,DSTEGR)(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(dstein,DSTEIN)(blasint* n, double* d, double* e, blasint* m, double* w, blasint* iblock, blasint* isplit, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info);

    void FC_GLOBAL(dstemr,DSTEMR)(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, blasint* m, double* w, double* z, blasint* ldz, blasint* nzc, blasint* isuppz, blaslogical* tryrac, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(dsteqr,DSTEQR)(char* compz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(dsterf,DSTERF)(blasint* n, double* d, double* e, blasint* info);

    void FC_GLOBAL(dstev,DSTEV)(char* jobz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_jobz);

    void FC_GLOBAL(dstevd,DSTEVD)(char* jobz, blasint* n, double* d, double* e, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz);

    void FC_GLOBAL(dstevr,DSTEVR)(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(dstevx,DSTEVX)(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(dsycon,DSYCON)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* anorm, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsyconv,DSYCONV)(char* uplo, char* way, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    void FC_GLOBAL(dsyequb,DSYEQUB)(char* uplo, blasint* n, double* a, blasint* lda, double* s, double* scond, double* amax, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsyev,DSYEV)(char* jobz, char* uplo, blasint* n, double* a, blasint* lda, double* w, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsyevd,DSYEVD)(char* jobz, char* uplo, blasint* n, double* a, blasint* lda, double* w, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsyevr,DSYEVR)(char* jobz, char* range, char* uplo, blasint* n, double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, blasint* isuppz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsyevx,DSYEVX)(char* jobz, char* range, char* uplo, blasint* n, double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsygs2,DSYGS2)(blasint* itype, char* uplo, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsygst,DSYGST)(blasint* itype, char* uplo, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsygv,DSYGV)(blasint* itype, char* jobz, char* uplo, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* w, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsygvd,DSYGVD)(blasint* itype, char* jobz, char* uplo, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* w, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsygvx,DSYGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* lwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsyrfs,DSYRFS)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* af, blasint* ldaf, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsysv,DSYSV)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsysvx,DSYSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, double* af, blasint* ldaf, blasint* ipiv, double* b, blasint* ldb, double* x, blasint* ldx, double* rcond, double* ferr, double* berr, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsyswapr,DSYSWAPR)(char* uplo, blasint* n, double* a, blasint* lda, blasint* i1, blasint* i2, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsytd2,DSYTD2)(char* uplo, blasint* n, double* a, blasint* lda, double* d, double* e, double* tau, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsytf2,DSYTF2)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsytrd,DSYTRD)(char* uplo, blasint* n, double* a, blasint* lda, double* d, double* e, double* tau, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsytrf,DSYTRF)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsytri,DSYTRI)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsytri2,DSYTRI2)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsytri2x,DSYTRI2X)(char* uplo, blasint* n, double* a, blasint* lda, blasint* ipiv, double* work, blasint* nb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsytrs,DSYTRS)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dsytrs2,DSYTRS2)(char* uplo, blasint* n, blasint* nrhs, double* a, blasint* lda, blasint* ipiv, double* b, blasint* ldb, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dtbcon,DTBCON)(char* norm, char* uplo, char* diag, blasint* n, blasint* kd, double* ab, blasint* ldab, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtbrfs,DTBRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtbtrs,DTBTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* kd, blasint* nrhs, double* ab, blasint* ldab, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtfsm,DTFSM)(char* transr, char* side, char* uplo, char* trans, char* diag, blasint* m, blasint* n, double* alpha, double* a, double* b, blasint* ldb, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtftri,DTFTRI)(char* transr, char* uplo, char* diag, blasint* n, double* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtfttp,DTFTTP)(char* transr, char* uplo, blasint* n, double* arf, double* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dtfttr,DTFTTR)(char* transr, char* uplo, blasint* n, double* arf, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dtgevc,DTGEVC)(char* side, char* howmny, blaslogical* selectfunc, blasint* n, double* s, blasint* lds, double* p, blasint* ldp, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(dtgex2,DTGEX2)(blaslogical* wantq, blaslogical* wantz, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, blasint* j1, blasint* n1, blasint* n2, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dtgexc,DTGEXC)(blaslogical* wantq, blaslogical* wantz, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, blasint* ifst, blasint* ilst, double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(dtgsen,DTGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* selectfunc, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* alphar, double* alphai, double* beta, double* q, blasint* ldq, double* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info);

    void FC_GLOBAL(dtgsja,DTGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, double* a, blasint* lda, double* b, blasint* ldb, double* tola, double* tolb, double* alpha, double* beta, double* u, blasint* ldu, double* v, blasint* ldv, double* q, blasint* ldq, double* work, blasint* ncycle, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(dtgsna,DTGSNA)(char* job, char* howmny, blaslogical* selectfunc, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* vl, blasint* ldvl, double* vr, blasint* ldvr, double* s, double* dif, blasint* mm, blasint* m, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(dtgsy2,DTGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* iwork, blasint* pq, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dtgsyl,DTGSYL)(char* trans, blasint* ijob, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* d, blasint* ldd, double* e, blasint* lde, double* f, blasint* ldf, double* scale, double* dif, double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dtpcon,DTPCON)(char* norm, char* uplo, char* diag, blasint* n, double* ap, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtpmqrt,DTPMQRT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, blasint* nb, double* v, blasint* ldv, double* t, blasint* ldt, double* a, blasint* lda, double* b, blasint* ldb, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(dtpqrt,DTPQRT)(blasint* m, blasint* n, blasint* l, blasint* nb, double* a, blasint* lda, double* b, blasint* ldb, double* t, blasint* ldt, double* work, blasint* info);

    void FC_GLOBAL(dtpqrt2,DTPQRT2)(blasint* m, blasint* n, blasint* l, double* a, blasint* lda, double* b, blasint* ldb, double* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(dtprfb,DTPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, double* v, blasint* ldv, double* t, blasint* ldt, double* a, blasint* lda, double* b, blasint* ldb, double* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(dtprfs,DTPRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, double* ap, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtptri,DTPTRI)(char* uplo, char* diag, blasint* n, double* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtptrs,DTPTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, double* ap, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtpttf,DTPTTF)(char* transr, char* uplo, blasint* n, double* ap, double* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dtpttr,DTPTTR)(char* uplo, blasint* n, double* ap, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dtrcon,DTRCON)(char* norm, char* uplo, char* diag, blasint* n, double* a, blasint* lda, double* rcond, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtrevc,DTREVC)(char* side, char* howmny, blaslogical* selectfunc, blasint* n, double* t, blasint* ldt, double* vl, blasint* ldvl, double* vr, blasint* ldvr, blasint* mm, blasint* m, double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(dtrexc,DTREXC)(char* compq, blasint* n, double* t, blasint* ldt, double* q, blasint* ldq, blasint* ifst, blasint* ilst, double* work, blasint* info, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(dtrrfs,DTRRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, double* x, blasint* ldx, double* ferr, double* berr, double* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtrsen,DTRSEN)(char* job, char* compq, blaslogical* selectfunc, blasint* n, double* t, blasint* ldt, double* q, blasint* ldq, double* wr, double* wi, blasint* m, double* s, double* sep, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(dtrsna,DTRSNA)(char* job, char* howmny, blaslogical* selectfunc, blasint* n, double* t, blasint* ldt, double* vl, blasint* ldvl, double* vr, blasint* ldvr, double* s, double* sep, blasint* mm, blasint* m, double* work, blasint* ldwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(dtrsyl,DTRSYL)(char* trana, char* tranb, blasint* isgn, blasint* m, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* c, blasint* ldc, double* scale, blasint* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb);

    void FC_GLOBAL(dtrti2,DTRTI2)(char* uplo, char* diag, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtrtri,DTRTRI)(char* uplo, char* diag, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtrtrs,DTRTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, double* a, blasint* lda, double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(dtrttf,DTRTTF)(char* transr, char* uplo, blasint* n, double* a, blasint* lda, double* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dtrttp,DTRTTP)(char* uplo, blasint* n, double* a, blasint* lda, double* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(dtzrqf,DTZRQF)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, blasint* info);

    void FC_GLOBAL(dtzrzf,DTZRZF)(blasint* m, blasint* n, double* a, blasint* lda, double* tau, double* work, blasint* lwork, blasint* info);

    double FC_GLOBAL(dzsum1,DZSUM1)(blasint* n, lapack_complex_double* cx, blasint* incx);

    blasint FC_GLOBAL(icmax1,ICMAX1)(blasint* n, lapack_complex_float* cx, blasint* incx);

    blasint FC_GLOBAL(ieeeck,IEEECK)(blasint* ispec, float* zero, float* one);

    blasint FC_GLOBAL(ilaclc,ILACLC)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda);

    blasint FC_GLOBAL(ilaclr,ILACLR)(blasint* m, blasint* n, lapack_complex_float* a, blasint* lda);

    blasint FC_GLOBAL(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag);

    blasint FC_GLOBAL(iladlc,ILADLC)(blasint* m, blasint* n, double* a, blasint* lda);

    blasint FC_GLOBAL(iladlr,ILADLR)(blasint* m, blasint* n, double* a, blasint* lda);

    blasint FC_GLOBAL(ilaenv,ILAENV)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);

    blasint FC_GLOBAL(ilaprec,ILAPREC)(char* prec, flexiblas_fortran_charlen_t len_prec);

    blasint FC_GLOBAL(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda);

    blasint FC_GLOBAL(ilaslr,ILASLR)(blasint* m, blasint* n, float* a, blasint* lda);

    blasint FC_GLOBAL(ilatrans,ILATRANS)(char* trans, flexiblas_fortran_charlen_t len_trans);

    blasint FC_GLOBAL(ilauplo,ILAUPLO)(char* uplo, flexiblas_fortran_charlen_t len_uplo);

    blasint FC_GLOBAL(ilazlc,ILAZLC)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda);

    blasint FC_GLOBAL(ilazlr,ILAZLR)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda);

    blasint FC_GLOBAL(iparmq,IPARMQ)(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);

    blasint FC_GLOBAL(izmax1,IZMAX1)(blasint* n, lapack_complex_double* cx, blasint* incx);

    void FC_GLOBAL(sbbcsd,SBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, float* theta, float* phi, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* v2t, blasint* ldv2t, float* b11d, float* b11e, float* b12d, float* b12e, float* b21d, float* b21e, float* b22d, float* b22e, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sbdsdc,SBDSDC)(char* uplo, char* compq, blasint* n, float* d, float* e, float* u, blasint* ldu, float* vt, blasint* ldvt, float* q, blasint* iq, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(sbdsqr,SBDSQR)(char* uplo, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, float* d, float* e, float* vt, blasint* ldvt, float* u, blasint* ldu, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(scsum1,SCSUM1)(blasint* n, lapack_complex_float* cx, blasint* incx);

    void FC_GLOBAL(sdisna,SDISNA)(char* job, blasint* m, blasint* n, float* d, float* sep, blasint* info, flexiblas_fortran_charlen_t len_job);

    float FC_GLOBAL(second,SECOND)(void);

    void FC_GLOBAL(sgbbrd,SGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* d, float* e, float* q, blasint* ldq, float* pt, blasint* ldpt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect);

    void FC_GLOBAL(sgbcon,SGBCON)(char* norm, blasint* n, blasint* kl, blasint* ku, float* ab, blasint* ldab, blasint* ipiv, float* anorm, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(sgbequ,SGBEQU)(blasint* m, blasint* n, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info);

    void FC_GLOBAL(sgbequb,SGBEQUB)(blasint* m, blasint* n, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info);

    void FC_GLOBAL(sgbrfs,SGBRFS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float* ab, blasint* ldab, float* afb, blasint* ldafb, blasint* ipiv, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sgbsv,SGBSV)(blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float* ab, blasint* ldab, blasint* ipiv, float* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(sgbsvx,SGBSVX)(char* fact, char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float* ab, blasint* ldab, float* afb, blasint* ldafb, blasint* ipiv, char* equed, float* r, float* c, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(sgbtf2,SGBTF2)(blasint* m, blasint* n, blasint* kl, blasint* ku, float* ab, blasint* ldab, blasint* ipiv, blasint* info);

    void FC_GLOBAL(sgbtrf,SGBTRF)(blasint* m, blasint* n, blasint* kl, blasint* ku, float* ab, blasint* ldab, blasint* ipiv, blasint* info);

    void FC_GLOBAL(sgbtrs,SGBTRS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, float* ab, blasint* ldab, blasint* ipiv, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sgebak,SGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, float* scale, blasint* m, float* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(sgebal,SGEBAL)(char* job, blasint* n, float* a, blasint* lda, blasint* ilo, blasint* ihi, float* scale, blasint* info, flexiblas_fortran_charlen_t len_job);

    void FC_GLOBAL(sgebd2,SGEBD2)(blasint* m, blasint* n, float* a, blasint* lda, float* d, float* e, float* tauq, float* taup, float* work, blasint* info);

    void FC_GLOBAL(sgebrd,SGEBRD)(blasint* m, blasint* n, float* a, blasint* lda, float* d, float* e, float* tauq, float* taup, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgecon,SGECON)(char* norm, blasint* n, float* a, blasint* lda, float* anorm, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(sgeequ,SGEEQU)(blasint* m, blasint* n, float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info);

    void FC_GLOBAL(sgeequb,SGEEQUB)(blasint* m, blasint* n, float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info);

    void FC_GLOBAL(sgees,SGEES)(char* jobvs, char* sort, blaslogical* selectfunc, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

    void FC_GLOBAL(sgeesx,SGEESX)(char* jobvs, char* sort, blaslogical* selectfunc, char* sense, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(sgeev,SGEEV)(char* jobvl, char* jobvr, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(sgeevx,SGEEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float* a, blasint* lda, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* scale, float* abnrm, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(sgegs,SGEGS)(char* jobvsl, char* jobvsr, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    void FC_GLOBAL(sgegv,SGEGV)(char* jobvl, char* jobvr, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(sgehd2,SGEHD2)(blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sgehrd,SGEHRD)(blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgejsv,SGEJSV)(char* joba, char* jobu, char* jobv, char* jobr, char* jobt, char* jobp, blasint* m, blasint* n, float* a, blasint* lda, float* sva, float* u, blasint* ldu, float* v, blasint* ldv, float* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobr, flexiblas_fortran_charlen_t len_jobt, flexiblas_fortran_charlen_t len_jobp);

    void FC_GLOBAL(sgelq2,SGELQ2)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sgelqf,SGELQF)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgels,SGELS)(char* trans, blasint* m, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sgelsd,SGELSD)(blasint* m, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, float* work, blasint* lwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(sgelss,SGELSS)(blasint* m, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, float* s, float* rcond, blasint* rank_bn, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgelsx,SGELSX)(blasint* m, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, blasint* jpvt, float* rcond, blasint* rank_bn, float* work, blasint* info);

    void FC_GLOBAL(sgelsy,SGELSY)(blasint* m, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, blasint* jpvt, float* rcond, blasint* rank_bn, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgemqrt,SGEMQRT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* nb, float* v, blasint* ldv, float* t, blasint* ldt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sgeql2,SGEQL2)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sgeqlf,SGEQLF)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgeqp3,SGEQP3)(blasint* m, blasint* n, float* a, blasint* lda, blasint* jpvt, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgeqpf,SGEQPF)(blasint* m, blasint* n, float* a, blasint* lda, blasint* jpvt, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sgeqr2,SGEQR2)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sgeqr2p,SGEQR2P)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sgeqrf,SGEQRF)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgeqrfp,SGEQRFP)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgeqrt,SGEQRT)(blasint* m, blasint* n, blasint* nb, float* a, blasint* lda, float* t, blasint* ldt, float* work, blasint* info);

    void FC_GLOBAL(sgeqrt2,SGEQRT2)(blasint* m, blasint* n, float* a, blasint* lda, float* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(sgeqrt3,SGEQRT3)(blasint* m, blasint* n, float* a, blasint* lda, float* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(sgerfs,SGERFS)(char* trans, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, blasint* ipiv, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sgerq2,SGERQ2)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sgerqf,SGERQF)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgesc2,SGESC2)(blasint* n, float* a, blasint* lda, float* rhs, blasint* ipiv, blasint* jpiv, float* scale);

    void FC_GLOBAL(sgesdd,SGESDD)(char* jobz, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* vt, blasint* ldvt, float* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz);

    void FC_GLOBAL(sgesv,SGESV)(blasint* n, blasint* nrhs, float* a, blasint* lda, blasint* ipiv, float* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(sgesvd,SGESVD)(char* jobu, char* jobvt, blasint* m, blasint* n, float* a, blasint* lda, float* s, float* u, blasint* ldu, float* vt, blasint* ldvt, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt);

    void FC_GLOBAL(sgesvj,SGESVJ)(char* joba, char* jobu, char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* sva, blasint* mv, float* v, blasint* ldv, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_joba, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv);

    void FC_GLOBAL(sgesvx,SGESVX)(char* fact, char* trans, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, blasint* ipiv, char* equed, float* r, float* c, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(sgetc2,SGETC2)(blasint* n, float* a, blasint* lda, blasint* ipiv, blasint* jpiv, blasint* info);

    void FC_GLOBAL(sgetf2,SGETF2)(blasint* m, blasint* n, float* a, blasint* lda, blasint* ipiv, blasint* info);

    void FC_GLOBAL(sgetrf,SGETRF)(blasint* m, blasint* n, float* a, blasint* lda, blasint* ipiv, blasint* info);

    void FC_GLOBAL(sgetri,SGETRI)(blasint* n, float* a, blasint* lda, blasint* ipiv, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgetrs,SGETRS)(char* trans, blasint* n, blasint* nrhs, float* a, blasint* lda, blasint* ipiv, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sggbak,SGGBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, float* lscale, float* rscale, blasint* m, float* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(sggbal,SGGBAL)(char* job, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* work, blasint* info, flexiblas_fortran_charlen_t len_job);

    void FC_GLOBAL(sgges,SGGES)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* work, blasint* lwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort);

    void FC_GLOBAL(sggesx,SGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* sdim, float* alphar, float* alphai, float* beta, float* vsl, blasint* ldvsl, float* vsr, blasint* ldvsr, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(sggev,SGGEV)(char* jobvl, char* jobvr, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(sggevx,SGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* ilo, blasint* ihi, float* lscale, float* rscale, float* abnrm, float* bbnrm, float* rconde, float* rcondv, float* work, blasint* lwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(sggglm,SGGGLM)(blasint* n, blasint* m, blasint* p, float* a, blasint* lda, float* b, blasint* ldb, float* d, float* x, float* y, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sgghrd,SGGHRD)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(sgglse,SGGLSE)(blasint* m, blasint* n, blasint* p, float* a, blasint* lda, float* b, blasint* ldb, float* c, float* d, float* x, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sggqrf,SGGQRF)(blasint* n, blasint* m, blasint* p, float* a, blasint* lda, float* taua, float* b, blasint* ldb, float* taub, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sggrqf,SGGRQF)(blasint* m, blasint* p, blasint* n, float* a, blasint* lda, float* taua, float* b, blasint* ldb, float* taub, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sggsvd,SGGSVD)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* n, blasint* p, blasint* k, blasint* l, float* a, blasint* lda, float* b, blasint* ldb, float* alpha, float* beta, float* u, blasint* ldu, float* v, blasint* ldv, float* q, blasint* ldq, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(sggsvp,SGGSVP)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* tola, float* tolb, blasint* k, blasint* l, float* u, blasint* ldu, float* v, blasint* ldv, float* q, blasint* ldq, blasint* iwork, float* tau, float* work, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(sgsvj0,SGSVJ0)(char* jobv, blasint* m, blasint* n, float* a, blasint* lda, float* d, float* sva, blasint* mv, float* v, blasint* ldv, float* eps, float* sfmin, float* tol, blasint* nsweep, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobv);

    void FC_GLOBAL(sgsvj1,SGSVJ1)(char* jobv, blasint* m, blasint* n, blasint* n1, float* a, blasint* lda, float* d, float* sva, blasint* mv, float* v, blasint* ldv, float* eps, float* sfmin, float* tol, blasint* nsweep, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobv);

    void FC_GLOBAL(sgtcon,SGTCON)(char* norm, blasint* n, float* dl, float* d, float* du, float* du2, blasint* ipiv, float* anorm, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(sgtrfs,SGTRFS)(char* trans, blasint* n, blasint* nrhs, float* dl, float* d, float* du, float* dlf, float* df, float* duf, float* du2, blasint* ipiv, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sgtsv,SGTSV)(blasint* n, blasint* nrhs, float* dl, float* d, float* du, float* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(sgtsvx,SGTSVX)(char* fact, char* trans, blasint* n, blasint* nrhs, float* dl, float* d, float* du, float* dlf, float* df, float* duf, float* du2, blasint* ipiv, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sgttrf,SGTTRF)(blasint* n, float* dl, float* d, float* du, float* du2, blasint* ipiv, blasint* info);

    void FC_GLOBAL(sgttrs,SGTTRS)(char* trans, blasint* n, blasint* nrhs, float* dl, float* d, float* du, float* du2, blasint* ipiv, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sgtts2,SGTTS2)(blasint* itrans, blasint* n, blasint* nrhs, float* dl, float* d, float* du, float* du2, blasint* ipiv, float* b, blasint* ldb);

    void FC_GLOBAL(shgeqz,SHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* t, blasint* ldt, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(shsein,SHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* selectfunc, blasint* n, float* h, blasint* ldh, float* wr, float* wi, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

    void FC_GLOBAL(shseqr,SHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* wr, float* wi, float* z, blasint* ldz, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz);

    blaslogical FC_GLOBAL(sisnan,SISNAN)(float* sin);

    void FC_GLOBAL(slabad,SLABAD)(float* small, float* large);

    void FC_GLOBAL(slabrd,SLABRD)(blasint* m, blasint* n, blasint* nb, float* a, blasint* lda, float* d, float* e, float* tauq, float* taup, float* x, blasint* ldx, float* y, blasint* ldy);

    void FC_GLOBAL(slacn2,SLACN2)(blasint* n, float* v, float* x, blasint* isgn, float* est, blasint* kase, blasint* isave);

    void FC_GLOBAL(slacon,SLACON)(blasint* n, float* v, float* x, blasint* isgn, float* est, blasint* kase);

    void FC_GLOBAL(slacpy,SLACPY)(char* uplo, blasint* m, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sladiv,SLADIV)(float* a, float* b, float* c, float* d, float* p, float* q);

    void FC_GLOBAL(slae2,SLAE2)(float* a, float* b, float* c, float* rt1, float* rt2);

    void FC_GLOBAL(slaebz,SLAEBZ)(blasint* ijob, blasint* nitmax, blasint* n, blasint* mmax, blasint* minp, blasint* nbmin, float* abstol, float* reltol, float* pivmin, float* d, float* e, float* e2, blasint* nval, float* ab, float* c, blasint* mout, blasint* nab, float* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(slaed0,SLAED0)(blasint* icompq, blasint* qsiz, blasint* n, float* d, float* e, float* q, blasint* ldq, float* qstore, blasint* ldqs, float* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(slaed1,SLAED1)(blasint* n, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(slaed2,SLAED2)(blasint* k, blasint* n, blasint* n1, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, float* z, float* dlamda, float* w, float* q2, blasint* indx, blasint* indxc, blasint* indxp, blasint* coltyp, blasint* info);

    void FC_GLOBAL(slaed3,SLAED3)(blasint* k, blasint* n, blasint* n1, float* d, float* q, blasint* ldq, float* rho, float* dlamda, float* q2, blasint* indx, blasint* ctot, float* w, float* s, blasint* info);

    void FC_GLOBAL(slaed4,SLAED4)(blasint* n, blasint* i, float* d, float* z, float* delta, float* rho, float* dlam, blasint* info);

    void FC_GLOBAL(slaed5,SLAED5)(blasint* i, float* d, float* z, float* delta, float* rho, float* dlam);

    void FC_GLOBAL(slaed6,SLAED6)(blasint* kniter, blaslogical* orgati, float* rho, float* d, float* z, float* finit, float* tau, blasint* info);

    void FC_GLOBAL(slaed7,SLAED7)(blasint* icompq, blasint* n, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(slaed8,SLAED8)(blasint* icompq, blasint* k, blasint* n, blasint* qsiz, float* d, float* q, blasint* ldq, blasint* indxq, float* rho, blasint* cutpnt, float* z, float* dlamda, float* q2, blasint* ldq2, float* w, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, blasint* indxp, blasint* indx, blasint* info);

    void FC_GLOBAL(slaed9,SLAED9)(blasint* k, blasint* kstart, blasint* kstop, blasint* n, float* d, float* q, blasint* ldq, float* rho, float* dlamda, float* w, float* s, blasint* lds, blasint* info);

    void FC_GLOBAL(slaeda,SLAEDA)(blasint* n, blasint* tlvls, blasint* curlvl, blasint* curpbm, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, float* givnum, float* q, blasint* qptr, float* z, float* ztemp, blasint* info);

    void FC_GLOBAL(slaein,SLAEIN)(blaslogical* rightv, blaslogical* noinit, blasint* n, float* h, blasint* ldh, float* wr, float* wi, float* vr, float* vi, float* b, blasint* ldb, float* work, float* eps3, float* smlnum, float* bignum, blasint* info);

    void FC_GLOBAL(slaev2,SLAEV2)(float* a, float* b, float* c, float* rt1, float* rt2, float* cs1, float* sn1);

    void FC_GLOBAL(slaexc,SLAEXC)(blaslogical* wantq, blasint* n, float* t, blasint* ldt, float* q, blasint* ldq, blasint* j1, blasint* n1, blasint* n2, float* work, blasint* info);

    void FC_GLOBAL(slag2,SLAG2)(float* a, blasint* lda, float* b, blasint* ldb, float* safmin, float* scale1, float* scale2, float* wr1, float* wr2, float* wi);

    void FC_GLOBAL(slag2d,SLAG2D)(blasint* m, blasint* n, float* sa, blasint* ldsa, double* a, blasint* lda, blasint* info);

    void FC_GLOBAL(slags2,SLAGS2)(blaslogical* upper, float* a1, float* a2, float* a3, float* b1, float* b2, float* b3, float* csu, float* snu, float* csv, float* snv, float* csq, float* snq);

    void FC_GLOBAL(slagtf,SLAGTF)(blasint* n, float* a, float* lambda, float* b, float* c, float* tol, float* d, blasint* in, blasint* info);

    void FC_GLOBAL(slagtm,SLAGTM)(char* trans, blasint* n, blasint* nrhs, float* alpha, float* dl, float* d, float* du, float* x, blasint* ldx, float* beta, float* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(slagts,SLAGTS)(blasint* job, blasint* n, float* a, float* b, float* c, float* d, blasint* in, float* y, float* tol, blasint* info);

    void FC_GLOBAL(slagv2,SLAGV2)(float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* csl, float* snl, float* csr, float* snr);

    void FC_GLOBAL(slahqr,SLAHQR)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* wr, float* wi, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, blasint* info);

    void FC_GLOBAL(slahr2,SLAHR2)(blasint* n, blasint* k, blasint* nb, float* a, blasint* lda, float* tau, float* t, blasint* ldt, float* y, blasint* ldy);

    void FC_GLOBAL(slahrd,SLAHRD)(blasint* n, blasint* k, blasint* nb, float* a, blasint* lda, float* tau, float* t, blasint* ldt, float* y, blasint* ldy);

    void FC_GLOBAL(slaic1,SLAIC1)(blasint* job, blasint* j, float* x, float* sest, float* w, float* gamma, float* sestpr, float* s, float* c);

    blaslogical FC_GLOBAL(slaisnan,SLAISNAN)(float* sin1, float* sin2);

    void FC_GLOBAL(slaln2,SLALN2)(blaslogical* ltrans, blasint* na, blasint* nw, float* smin, float* ca, float* a, blasint* lda, float* d1, float* d2, float* b, blasint* ldb, float* wr, float* wi, float* x, blasint* ldx, float* scale, float* xnorm, blasint* info);

    void FC_GLOBAL(slals0,SLALS0)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, float* b, blasint* ldb, float* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* poles, float* difl, float* difr, float* z, blasint* k, float* c, float* s, float* work, blasint* info);

    void FC_GLOBAL(slalsa,SLALSA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* nrhs, float* b, blasint* ldb, float* bx, blasint* ldbx, float* u, blasint* ldu, float* vt, blasint* k, float* difl, float* difr, float* z, float* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, float* givnum, float* c, float* s, float* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(slalsd,SLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, float* d, float* e, float* b, blasint* ldb, float* rcond, blasint* rank_bn, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(slamc3,SLAMC3)(float* a, float* b);

    float FC_GLOBAL(slamch,SLAMCH)(char* cmach, flexiblas_fortran_charlen_t len_cmach);

    void FC_GLOBAL(slamrg,SLAMRG)(blasint* n1, blasint* n2, float* a, blasint* strd1, blasint* strd2, blasint* index_bn);

    blasint FC_GLOBAL(slaneg,SLANEG)(blasint* n, float* d, float* lld, float* sigma, float* pivmin, blasint* r);

    float FC_GLOBAL(slangb,SLANGB)(char* norm, blasint* n, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* work, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(slange,SLANGE)(char* norm, blasint* m, blasint* n, float* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(slangt,SLANGT)(char* norm, blasint* n, float* dl, float* d, float* du, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(slanhs,SLANHS)(char* norm, blasint* n, float* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(slansb,SLANSB)(char* norm, char* uplo, blasint* n, blasint* k, float* ab, blasint* ldab, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(slansf,SLANSF)(char* norm, char* transr, char* uplo, blasint* n, float* a, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(slansp,SLANSP)(char* norm, char* uplo, blasint* n, float* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(slanst,SLANST)(char* norm, blasint* n, float* d, float* e, flexiblas_fortran_charlen_t len_norm);

    float FC_GLOBAL(slansy,SLANSY)(char* norm, char* uplo, blasint* n, float* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    float FC_GLOBAL(slantb,SLANTB)(char* norm, char* uplo, char* diag, blasint* n, blasint* k, float* ab, blasint* ldab, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    float FC_GLOBAL(slantp,SLANTP)(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    float FC_GLOBAL(slantr,SLANTR)(char* norm, char* uplo, char* diag, blasint* m, blasint* n, float* a, blasint* lda, float* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(slanv2,SLANV2)(float* a, float* b, float* c, float* d, float* rt1r, float* rt1i, float* rt2r, float* rt2i, float* cs, float* sn);

    void FC_GLOBAL(slapll,SLAPLL)(blasint* n, float* x, blasint* incx, float* y, blasint* incy, float* ssmin);

    void FC_GLOBAL(slapmr,SLAPMR)(blaslogical* forwrd, blasint* m, blasint* n, float* x, blasint* ldx, blasint* k);

    void FC_GLOBAL(slapmt,SLAPMT)(blaslogical* forwrd, blasint* m, blasint* n, float* x, blasint* ldx, blasint* k);

    float FC_GLOBAL(slapy2,SLAPY2)(float* x, float* y);

    float FC_GLOBAL(slapy3,SLAPY3)(float* x, float* y, float* z);

    void FC_GLOBAL(slaqgb,SLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* r, float* c, float* rowcnd, float* colcnd, float* amax, char* equed, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(slaqge,SLAQGE)(blasint* m, blasint* n, float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, char* equed, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(slaqp2,SLAQP2)(blasint* m, blasint* n, blasint* offset, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* work);

    void FC_GLOBAL(slaqps,SLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* auxv, float* f, blasint* ldf);

    void FC_GLOBAL(slaqr0,SLAQR0)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* wr, float* wi, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(slaqr1,SLAQR1)(blasint* n, float* h, blasint* ldh, float* sr1, float* si1, float* sr2, float* si2, float* v);

    void FC_GLOBAL(slaqr2,SLAQR2)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float* h, blasint* ldh, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, blasint* ns, blasint* nd, float* sr, float* si, float* v, blasint* ldv, blasint* nh, float* t, blasint* ldt, blasint* nv, float* wv, blasint* ldwv, float* work, blasint* lwork);

    void FC_GLOBAL(slaqr3,SLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, float* h, blasint* ldh, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, blasint* ns, blasint* nd, float* sr, float* si, float* v, blasint* ldv, blasint* nh, float* t, blasint* ldt, blasint* nv, float* wv, blasint* ldwv, float* work, blasint* lwork);

    void FC_GLOBAL(slaqr4,SLAQR4)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* wr, float* wi, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(slaqr5,SLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, float* sr, float* si, float* h, blasint* ldh, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, float* v, blasint* ldv, float* u, blasint* ldu, blasint* nv, float* wv, blasint* ldwv, blasint* nh, float* wh, blasint* ldwh);

    void FC_GLOBAL(slaqsb,SLAQSB)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(slaqsp,SLAQSP)(char* uplo, blasint* n, float* ap, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(slaqsy,SLAQSY)(char* uplo, blasint* n, float* a, blasint* lda, float* s, float* scond, float* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(slaqtr,SLAQTR)(blaslogical* ltran, blaslogical* lreal, blasint* n, float* t, blasint* ldt, float* b, float* w, float* scale, float* x, float* work, blasint* info);

    void FC_GLOBAL(slar1v,SLAR1V)(blasint* n, blasint* b1, blasint* bn, float* lambda, float* d, float* l, float* ld, float* lld, float* pivmin, float* gaptol, float* z, blaslogical* wantnc, blasint* negcnt, float* ztz, float* mingma, blasint* r, blasint* isuppz, float* nrminv, float* resid, float* rqcorr, float* work);

    void FC_GLOBAL(slar2v,SLAR2V)(blasint* n, float* x, float* y, float* z, blasint* incx, float* c, float* s, blasint* incc);

    void FC_GLOBAL(slarf,SLARF)(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c, blasint* ldc, float* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(slarfb,SLARFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, float* v, blasint* ldv, float* t, blasint* ldt, float* c, blasint* ldc, float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(slarfg,SLARFG)(blasint* n, float* alpha, float* x, blasint* incx, float* tau);

    void FC_GLOBAL(slarfgp,SLARFGP)(blasint* n, float* alpha, float* x, blasint* incx, float* tau);

    void FC_GLOBAL(slarft,SLARFT)(char* direct, char* storev, blasint* n, blasint* k, float* v, blasint* ldv, float* tau, float* t, blasint* ldt, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(slarfx,SLARFX)(char* side, blasint* m, blasint* n, float* v, float* tau, float* c, blasint* ldc, float* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(slargv,SLARGV)(blasint* n, float* x, blasint* incx, float* y, blasint* incy, float* c, blasint* incc);

    void FC_GLOBAL(slarnv,SLARNV)(blasint* idist, blasint* iseed, blasint* n, float* x);

    void FC_GLOBAL(slarra,SLARRA)(blasint* n, float* d, float* e, float* e2, float* spltol, float* tnrm, blasint* nsplit, blasint* isplit, blasint* info);

    void FC_GLOBAL(slarrb,SLARRB)(blasint* n, float* d, float* lld, blasint* ifirst, blasint* ilast, float* rtol1, float* rtol2, blasint* offset, float* w, float* wgap, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* twist, blasint* info);

    void FC_GLOBAL(slarrc,SLARRC)(char* jobt, blasint* n, float* vl, float* vu, float* d, float* e, float* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt);

    void FC_GLOBAL(slarrd,SLARRD)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* gers, float* reltol, float* d, float* e, float* e2, float* pivmin, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wl, float* wu, blasint* iblock, blasint* indexw, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

    void FC_GLOBAL(slarre,SLARRE)(char* range, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* d, float* e, float* e2, float* rtol1, float* rtol2, float* spltol, blasint* nsplit, blasint* isplit, blasint* m, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* pivmin, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(slarrf,SLARRF)(blasint* n, float* d, float* l, float* ld, blasint* clstrt, blasint* clend, float* w, float* wgap, float* werr, float* spdiam, float* clgapl, float* clgapr, float* pivmin, float* sigma, float* dplus, float* lplus, float* work, blasint* info);

    void FC_GLOBAL(slarrj,SLARRJ)(blasint* n, float* d, float* e2, blasint* ifirst, blasint* ilast, float* rtol, blasint* offset, float* w, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* info);

    void FC_GLOBAL(slarrk,SLARRK)(blasint* n, blasint* iw, float* gl, float* gu, float* d, float* e2, float* pivmin, float* reltol, float* w, float* werr, blasint* info);

    void FC_GLOBAL(slarrr,SLARRR)(blasint* n, float* d, float* e, blasint* info);

    void FC_GLOBAL(slarrv,SLARRV)(blasint* n, float* vl, float* vu, float* d, float* l, float* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, float* minrgp, float* rtol1, float* rtol2, float* w, float* werr, float* wgap, blasint* iblock, blasint* indexw, float* gers, float* z, blasint* ldz, blasint* isuppz, float* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(slartg,SLARTG)(float* f, float* g, float* cs, float* sn, float* r);

    void FC_GLOBAL(slartgp,SLARTGP)(float* f, float* g, float* cs, float* sn, float* r);

    void FC_GLOBAL(slartgs,SLARTGS)(float* x, float* y, float* sigma, float* cs, float* sn);

    void FC_GLOBAL(slartv,SLARTV)(blasint* n, float* x, blasint* incx, float* y, blasint* incy, float* c, float* s, blasint* incc);

    void FC_GLOBAL(slaruv,SLARUV)(blasint* iseed, blasint* n, float* x);

    void FC_GLOBAL(slarz,SLARZ)(char* side, blasint* m, blasint* n, blasint* l, float* v, blasint* incv, float* tau, float* c, blasint* ldc, float* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(slarzb,SLARZB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float* v, blasint* ldv, float* t, blasint* ldt, float* c, blasint* ldc, float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(slarzt,SLARZT)(char* direct, char* storev, blasint* n, blasint* k, float* v, blasint* ldv, float* tau, float* t, blasint* ldt, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(slas2,SLAS2)(float* f, float* g, float* h, float* ssmin, float* ssmax);

    void FC_GLOBAL(slascl,SLASCL)(char* type_bn, blasint* kl, blasint* ku, float* cfrom, float* cto, blasint* m, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn);

    void FC_GLOBAL(slasd0,SLASD0)(blasint* n, blasint* sqre, float* d, float* e, float* u, blasint* ldu, float* vt, blasint* ldvt, blasint* smlsiz, blasint* iwork, float* work, blasint* info);

    void FC_GLOBAL(slasd1,SLASD1)(blasint* nl, blasint* nr, blasint* sqre, float* d, float* alpha, float* beta, float* u, blasint* ldu, float* vt, blasint* ldvt, blasint* idxq, blasint* iwork, float* work, blasint* info);

    void FC_GLOBAL(slasd2,SLASD2)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* alpha, float* beta, float* u, blasint* ldu, float* vt, blasint* ldvt, float* dsigma, float* u2, blasint* ldu2, float* vt2, blasint* ldvt2, blasint* idxp, blasint* idx, blasint* idxc, blasint* idxq, blasint* coltyp, blasint* info);

    void FC_GLOBAL(slasd3,SLASD3)(blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* q, blasint* ldq, float* dsigma, float* u, blasint* ldu, float* u2, blasint* ldu2, float* vt, blasint* ldvt, float* vt2, blasint* ldvt2, blasint* idxc, blasint* ctot, float* z, blasint* info);

    void FC_GLOBAL(slasd4,SLASD4)(blasint* n, blasint* i, float* d, float* z, float* delta, float* rho, float* sigma, float* work, blasint* info);

    void FC_GLOBAL(slasd5,SLASD5)(blasint* i, float* d, float* z, float* delta, float* rho, float* dsigma, float* work);

    void FC_GLOBAL(slasd6,SLASD6)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, float* d, float* vf, float* vl, float* alpha, float* beta, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* poles, float* difl, float* difr, float* z, blasint* k, float* c, float* s, float* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(slasd7,SLASD7)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* k, float* d, float* z, float* zw, float* vf, float* vfw, float* vl, float* vlw, float* alpha, float* beta, float* dsigma, blasint* idx, blasint* idxp, blasint* idxq, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, float* givnum, blasint* ldgnum, float* c, float* s, blasint* info);

    void FC_GLOBAL(slasd8,SLASD8)(blasint* icompq, blasint* k, float* d, float* z, float* vf, float* vl, float* difl, float* difr, blasint* lddifr, float* dsigma, float* work, blasint* info);

    void FC_GLOBAL(slasda,SLASDA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* sqre, float* d, float* e, float* u, blasint* ldu, float* vt, blasint* k, float* difl, float* difr, float* z, float* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, float* givnum, float* c, float* s, float* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(slasdq,SLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, float* d, float* e, float* vt, blasint* ldvt, float* u, blasint* ldu, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(slasdt,SLASDT)(blasint* n, blasint* lvl, blasint* nd, blasint* inode, blasint* ndiml, blasint* ndimr, blasint* msub);

    void FC_GLOBAL(slaset,SLASET)(char* uplo, blasint* m, blasint* n, float* alpha, float* beta, float* a, blasint* lda, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(slasq1,SLASQ1)(blasint* n, float* d, float* e, float* work, blasint* info);

    void FC_GLOBAL(slasq2,SLASQ2)(blasint* n, float* z, blasint* info);

    void FC_GLOBAL(slasq3,SLASQ3)(blasint* i0, blasint* n0, float* z, blasint* pp, float* dmin, float* sigma, float* desig, float* qmax, blasint* nfail, blasint* iter, blasint* ndiv, blaslogical* ieee, blasint* ttype, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* g, float* tau);

    void FC_GLOBAL(slasq4,SLASQ4)(blasint* i0, blasint* n0, float* z, blasint* pp, blasint* n0in, float* dmin, float* dmin1, float* dmin2, float* dn, float* dn1, float* dn2, float* tau, blasint* ttype, float* g);

    void FC_GLOBAL(slasq5,SLASQ5)(blasint* i0, blasint* n0, float* z, blasint* pp, float* tau, float* sigma, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2, blaslogical* ieee, float* eps);

    void FC_GLOBAL(slasq6,SLASQ6)(blasint* i0, blasint* n0, float* z, blasint* pp, float* dmin, float* dmin1, float* dmin2, float* dn, float* dnm1, float* dnm2);

    void FC_GLOBAL(slasr,SLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, float* c, float* s, float* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct);

    void FC_GLOBAL(slasrt,SLASRT)(char* id, blasint* n, float* d, blasint* info, flexiblas_fortran_charlen_t len_id);

    void FC_GLOBAL(slassq,SLASSQ)(blasint* n, float* x, blasint* incx, float* scale, float* sumsq);

    void FC_GLOBAL(slasv2,SLASV2)(float* f, float* g, float* h, float* ssmin, float* ssmax, float* snr, float* csr, float* snl, float* csl);

    void FC_GLOBAL(slaswp,SLASWP)(blasint* n, float* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx);

    void FC_GLOBAL(slasy2,SLASY2)(blaslogical* ltranl, blaslogical* ltranr, blasint* isgn, blasint* n1, blasint* n2, float* tl, blasint* ldtl, float* tr, blasint* ldtr, float* b, blasint* ldb, float* scale, float* x, blasint* ldx, float* xnorm, blasint* info);

    void FC_GLOBAL(slasyf,SLASYF)(char* uplo, blasint* n, blasint* nb, blasint* kb, float* a, blasint* lda, blasint* ipiv, float* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(slatbs,SLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, float* ab, blasint* ldab, float* x, float* scale, float* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(slatdf,SLATDF)(blasint* ijob, blasint* n, float* z, blasint* ldz, float* rhs, float* rdsum, float* rdscal, blasint* ipiv, blasint* jpiv);

    void FC_GLOBAL(slatps,SLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, float* ap, float* x, float* scale, float* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(slatrd,SLATRD)(char* uplo, blasint* n, blasint* nb, float* a, blasint* lda, float* e, float* tau, float* w, blasint* ldw, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(slatrs,SLATRS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, float* a, blasint* lda, float* x, float* scale, float* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(slatrz,SLATRZ)(blasint* m, blasint* n, blasint* l, float* a, blasint* lda, float* tau, float* work);

    void FC_GLOBAL(slatzm,SLATZM)(char* side, blasint* m, blasint* n, float* v, blasint* incv, float* tau, float* c1, float* c2, blasint* ldc, float* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(slauu2,SLAUU2)(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(slauum,SLAUUM)(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sopgtr,SOPGTR)(char* uplo, blasint* n, float* ap, float* tau, float* q, blasint* ldq, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sopmtr,SOPMTR)(char* side, char* uplo, char* trans, blasint* m, blasint* n, float* ap, float* tau, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sorbdb,SORBDB)(char* trans, char* signs, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x12, blasint* ldx12, float* x21, blasint* ldx21, float* x22, blasint* ldx22, float* theta, float* phi, float* taup1, float* taup2, float* tauq1, float* tauq2, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    void FC_GLOBAL(sorcsd,SORCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, float* x11, blasint* ldx11, float* x12, blasint* ldx12, float* x21, blasint* ldx21, float* x22, blasint* ldx22, float* theta, float* u1, blasint* ldu1, float* u2, blasint* ldu2, float* v1t, blasint* ldv1t, float* v2t, blasint* ldv2t, float* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    void FC_GLOBAL(sorg2l,SORG2L)(blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sorg2r,SORG2R)(blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sorgbr,SORGBR)(char* vect, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect);

    void FC_GLOBAL(sorghr,SORGHR)(blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sorgl2,SORGL2)(blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sorglq,SORGLQ)(blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sorgql,SORGQL)(blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sorgqr,SORGQR)(blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sorgr2,SORGR2)(blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* info);

    void FC_GLOBAL(sorgrq,SORGRQ)(blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(sorgtr,SORGTR)(char* uplo, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sorm2l,SORM2L)(char* side, char* trans, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sorm2r,SORM2R)(char* side, char* trans, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormbr,SORMBR)(char* vect, char* side, char* trans, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormhr,SORMHR)(char* side, char* trans, blasint* m, blasint* n, blasint* ilo, blasint* ihi, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sorml2,SORML2)(char* side, char* trans, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormlq,SORMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormql,SORMQL)(char* side, char* trans, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormqr,SORMQR)(char* side, char* trans, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormr2,SORMR2)(char* side, char* trans, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormr3,SORMR3)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormrq,SORMRQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormrz,SORMRZ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sormtr,SORMTR)(char* side, char* uplo, char* trans, blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* c, blasint* ldc, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(spbcon,SPBCON)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* anorm, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spbequ,SPBEQU)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* s, float* scond, float* amax, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spbrfs,SPBRFS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, float* ab, blasint* ldab, float* afb, blasint* ldafb, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spbstf,SPBSTF)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spbsv,SPBSV)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, float* ab, blasint* ldab, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spbsvx,SPBSVX)(char* fact, char* uplo, blasint* n, blasint* kd, blasint* nrhs, float* ab, blasint* ldab, float* afb, blasint* ldafb, char* equed, float* s, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(spbtf2,SPBTF2)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spbtrf,SPBTRF)(char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spbtrs,SPBTRS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, float* ab, blasint* ldab, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spftrf,SPFTRF)(char* transr, char* uplo, blasint* n, float* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spftri,SPFTRI)(char* transr, char* uplo, blasint* n, float* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spftrs,SPFTRS)(char* transr, char* uplo, blasint* n, blasint* nrhs, float* a, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spocon,SPOCON)(char* uplo, blasint* n, float* a, blasint* lda, float* anorm, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spoequ,SPOEQU)(blasint* n, float* a, blasint* lda, float* s, float* scond, float* amax, blasint* info);

    void FC_GLOBAL(spoequb,SPOEQUB)(blasint* n, float* a, blasint* lda, float* s, float* scond, float* amax, blasint* info);

    void FC_GLOBAL(sporfs,SPORFS)(char* uplo, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sposv,SPOSV)(char* uplo, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sposvx,SPOSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, char* equed, float* s, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(spotf2,SPOTF2)(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spotrf,SPOTRF)(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spotri,SPOTRI)(char* uplo, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spotrs,SPOTRS)(char* uplo, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sppcon,SPPCON)(char* uplo, blasint* n, float* ap, float* anorm, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sppequ,SPPEQU)(char* uplo, blasint* n, float* ap, float* s, float* scond, float* amax, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spprfs,SPPRFS)(char* uplo, blasint* n, blasint* nrhs, float* ap, float* afp, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sppsv,SPPSV)(char* uplo, blasint* n, blasint* nrhs, float* ap, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sppsvx,SPPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, float* ap, float* afp, char* equed, float* s, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(spptrf,SPPTRF)(char* uplo, blasint* n, float* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spptri,SPPTRI)(char* uplo, blasint* n, float* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spptrs,SPPTRS)(char* uplo, blasint* n, blasint* nrhs, float* ap, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spstf2,SPSTF2)(char* uplo, blasint* n, float* a, blasint* lda, blasint* piv, blasint* rank_bn, float* tol, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(spstrf,SPSTRF)(char* uplo, blasint* n, float* a, blasint* lda, blasint* piv, blasint* rank_bn, float* tol, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sptcon,SPTCON)(blasint* n, float* d, float* e, float* anorm, float* rcond, float* work, blasint* info);

    void FC_GLOBAL(spteqr,SPTEQR)(char* compz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(sptrfs,SPTRFS)(blasint* n, blasint* nrhs, float* d, float* e, float* df, float* ef, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* info);

    void FC_GLOBAL(sptsv,SPTSV)(blasint* n, blasint* nrhs, float* d, float* e, float* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(sptsvx,SPTSVX)(char* fact, blasint* n, blasint* nrhs, float* d, float* e, float* df, float* ef, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* info, flexiblas_fortran_charlen_t len_fact);

    void FC_GLOBAL(spttrf,SPTTRF)(blasint* n, float* d, float* e, blasint* info);

    void FC_GLOBAL(spttrs,SPTTRS)(blasint* n, blasint* nrhs, float* d, float* e, float* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(sptts2,SPTTS2)(blasint* n, blasint* nrhs, float* d, float* e, float* b, blasint* ldb);

    void FC_GLOBAL(srscl,SRSCL)(blasint* n, float* sa, float* sx, blasint* incx);

    void FC_GLOBAL(ssbev,SSBEV)(char* jobz, char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* w, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssbevd,SSBEVD)(char* jobz, char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* w, float* z, blasint* ldz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssbevx,SSBEVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* q, blasint* ldq, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssbgst,SSBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* x, blasint* ldx, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssbgv,SSBGV)(char* jobz, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* w, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssbgvd,SSBGVD)(char* jobz, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* w, float* z, blasint* ldz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssbgvx,SSBGVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* ka, blasint* kb, float* ab, blasint* ldab, float* bb, blasint* ldbb, float* q, blasint* ldq, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssbtrd,SSBTRD)(char* vect, char* uplo, blasint* n, blasint* kd, float* ab, blasint* ldab, float* d, float* e, float* q, blasint* ldq, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssfrk,SSFRK)(char* transr, char* uplo, char* trans, blasint* n, blasint* k, float* alpha, float* a, blasint* lda, float* beta, float* c, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(sspcon,SSPCON)(char* uplo, blasint* n, float* ap, blasint* ipiv, float* anorm, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sspev,SSPEV)(char* jobz, char* uplo, blasint* n, float* ap, float* w, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sspevd,SSPEVD)(char* jobz, char* uplo, blasint* n, float* ap, float* w, float* z, blasint* ldz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sspevx,SSPEVX)(char* jobz, char* range, char* uplo, blasint* n, float* ap, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sspgst,SSPGST)(blasint* itype, char* uplo, blasint* n, float* ap, float* bp, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sspgv,SSPGV)(blasint* itype, char* jobz, char* uplo, blasint* n, float* ap, float* bp, float* w, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sspgvd,SSPGVD)(blasint* itype, char* jobz, char* uplo, blasint* n, float* ap, float* bp, float* w, float* z, blasint* ldz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sspgvx,SSPGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, float* ap, float* bp, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssprfs,SSPRFS)(char* uplo, blasint* n, blasint* nrhs, float* ap, float* afp, blasint* ipiv, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sspsv,SSPSV)(char* uplo, blasint* n, blasint* nrhs, float* ap, blasint* ipiv, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sspsvx,SSPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, float* ap, float* afp, blasint* ipiv, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssptrd,SSPTRD)(char* uplo, blasint* n, float* ap, float* d, float* e, float* tau, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssptrf,SSPTRF)(char* uplo, blasint* n, float* ap, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssptri,SSPTRI)(char* uplo, blasint* n, float* ap, blasint* ipiv, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssptrs,SSPTRS)(char* uplo, blasint* n, blasint* nrhs, float* ap, blasint* ipiv, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(sstebz,SSTEBZ)(char* range, char* order, blasint* n, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, float* d, float* e, blasint* m, blasint* nsplit, float* w, blasint* iblock, blasint* isplit, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_order);

    void FC_GLOBAL(sstedc,SSTEDC)(char* compz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(sstegr,SSTEGR)(char* jobz, char* range, blasint* n, float* d, float* e, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, blasint* isuppz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(sstein,SSTEIN)(blasint* n, float* d, float* e, blasint* m, float* w, blasint* iblock, blasint* isplit, float* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info);

    void FC_GLOBAL(sstemr,SSTEMR)(char* jobz, char* range, blasint* n, float* d, float* e, float* vl, float* vu, blasint* il, blasint* iu, blasint* m, float* w, float* z, blasint* ldz, blasint* nzc, blasint* isuppz, blaslogical* tryrac, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(ssteqr,SSTEQR)(char* compz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(ssterf,SSTERF)(blasint* n, float* d, float* e, blasint* info);

    void FC_GLOBAL(sstev,SSTEV)(char* jobz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* info, flexiblas_fortran_charlen_t len_jobz);

    void FC_GLOBAL(sstevd,SSTEVD)(char* jobz, blasint* n, float* d, float* e, float* z, blasint* ldz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz);

    void FC_GLOBAL(sstevr,SSTEVR)(char* jobz, char* range, blasint* n, float* d, float* e, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, blasint* isuppz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(sstevx,SSTEVX)(char* jobz, char* range, blasint* n, float* d, float* e, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(ssycon,SSYCON)(char* uplo, blasint* n, float* a, blasint* lda, blasint* ipiv, float* anorm, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssyconv,SSYCONV)(char* uplo, char* way, blasint* n, float* a, blasint* lda, blasint* ipiv, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    void FC_GLOBAL(ssyequb,SSYEQUB)(char* uplo, blasint* n, float* a, blasint* lda, float* s, float* scond, float* amax, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssyev,SSYEV)(char* jobz, char* uplo, blasint* n, float* a, blasint* lda, float* w, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssyevd,SSYEVD)(char* jobz, char* uplo, blasint* n, float* a, blasint* lda, float* w, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssyevr,SSYEVR)(char* jobz, char* range, char* uplo, blasint* n, float* a, blasint* lda, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, blasint* isuppz, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssyevx,SSYEVX)(char* jobz, char* range, char* uplo, blasint* n, float* a, blasint* lda, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, float* work, blasint* lwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssygs2,SSYGS2)(blasint* itype, char* uplo, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssygst,SSYGST)(blasint* itype, char* uplo, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssygv,SSYGV)(blasint* itype, char* jobz, char* uplo, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* w, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssygvd,SSYGVD)(blasint* itype, char* jobz, char* uplo, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* w, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssygvx,SSYGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float* z, blasint* ldz, float* work, blasint* lwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssyrfs,SSYRFS)(char* uplo, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, blasint* ipiv, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssysv,SSYSV)(char* uplo, blasint* n, blasint* nrhs, float* a, blasint* lda, blasint* ipiv, float* b, blasint* ldb, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssysvx,SSYSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, float* a, blasint* lda, float* af, blasint* ldaf, blasint* ipiv, float* b, blasint* ldb, float* x, blasint* ldx, float* rcond, float* ferr, float* berr, float* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssyswapr,SSYSWAPR)(char* uplo, blasint* n, float* a, blasint* lda, blasint* i1, blasint* i2, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssytd2,SSYTD2)(char* uplo, blasint* n, float* a, blasint* lda, float* d, float* e, float* tau, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssytf2,SSYTF2)(char* uplo, blasint* n, float* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssytrd,SSYTRD)(char* uplo, blasint* n, float* a, blasint* lda, float* d, float* e, float* tau, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssytrf,SSYTRF)(char* uplo, blasint* n, float* a, blasint* lda, blasint* ipiv, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssytri,SSYTRI)(char* uplo, blasint* n, float* a, blasint* lda, blasint* ipiv, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssytri2,SSYTRI2)(char* uplo, blasint* n, float* a, blasint* lda, blasint* ipiv, float* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssytri2x,SSYTRI2X)(char* uplo, blasint* n, float* a, blasint* lda, blasint* ipiv, float* work, blasint* nb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssytrs,SSYTRS)(char* uplo, blasint* n, blasint* nrhs, float* a, blasint* lda, blasint* ipiv, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ssytrs2,SSYTRS2)(char* uplo, blasint* n, blasint* nrhs, float* a, blasint* lda, blasint* ipiv, float* b, blasint* ldb, float* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(stbcon,STBCON)(char* norm, char* uplo, char* diag, blasint* n, blasint* kd, float* ab, blasint* ldab, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(stbrfs,STBRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* kd, blasint* nrhs, float* ab, blasint* ldab, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(stbtrs,STBTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* kd, blasint* nrhs, float* ab, blasint* ldab, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(stfsm,STFSM)(char* transr, char* side, char* uplo, char* trans, char* diag, blasint* m, blasint* n, float* alpha, float* a, float* b, blasint* ldb, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(stftri,STFTRI)(char* transr, char* uplo, char* diag, blasint* n, float* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(stfttp,STFTTP)(char* transr, char* uplo, blasint* n, float* arf, float* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(stfttr,STFTTR)(char* transr, char* uplo, blasint* n, float* arf, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(stgevc,STGEVC)(char* side, char* howmny, blaslogical* selectfunc, blasint* n, float* s, blasint* lds, float* p, blasint* ldp, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(stgex2,STGEX2)(blaslogical* wantq, blaslogical* wantz, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* j1, blasint* n1, blasint* n2, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(stgexc,STGEXC)(blaslogical* wantq, blaslogical* wantz, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* ifst, blasint* ilst, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(stgsen,STGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* selectfunc, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* alphar, float* alphai, float* beta, float* q, blasint* ldq, float* z, blasint* ldz, blasint* m, float* pl, float* pr, float* dif, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info);

    void FC_GLOBAL(stgsja,STGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float* a, blasint* lda, float* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float* u, blasint* ldu, float* v, blasint* ldv, float* q, blasint* ldq, float* work, blasint* ncycle, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(stgsna,STGSNA)(char* job, char* howmny, blaslogical* selectfunc, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* s, float* dif, blasint* mm, blasint* m, float* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(stgsy2,STGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* c, blasint* ldc, float* d, blasint* ldd, float* e, blasint* lde, float* f, blasint* ldf, float* scale, float* rdsum, float* rdscal, blasint* iwork, blasint* pq, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(stgsyl,STGSYL)(char* trans, blasint* ijob, blasint* m, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* c, blasint* ldc, float* d, blasint* ldd, float* e, blasint* lde, float* f, blasint* ldf, float* scale, float* dif, float* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(stpcon,STPCON)(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(stpmqrt,STPMQRT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, blasint* nb, float* v, blasint* ldv, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(stpqrt,STPQRT)(blasint* m, blasint* n, blasint* l, blasint* nb, float* a, blasint* lda, float* b, blasint* ldb, float* t, blasint* ldt, float* work, blasint* info);

    void FC_GLOBAL(stpqrt2,STPQRT2)(blasint* m, blasint* n, blasint* l, float* a, blasint* lda, float* b, blasint* ldb, float* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(stprfb,STPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, float* v, blasint* ldv, float* t, blasint* ldt, float* a, blasint* lda, float* b, blasint* ldb, float* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(stprfs,STPRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* ap, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(stptri,STPTRI)(char* uplo, char* diag, blasint* n, float* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(stptrs,STPTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* ap, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(stpttf,STPTTF)(char* transr, char* uplo, blasint* n, float* ap, float* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(stpttr,STPTTR)(char* uplo, blasint* n, float* ap, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(strcon,STRCON)(char* norm, char* uplo, char* diag, blasint* n, float* a, blasint* lda, float* rcond, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(strevc,STREVC)(char* side, char* howmny, blaslogical* selectfunc, blasint* n, float* t, blasint* ldt, float* vl, blasint* ldvl, float* vr, blasint* ldvr, blasint* mm, blasint* m, float* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(strexc,STREXC)(char* compq, blasint* n, float* t, blasint* ldt, float* q, blasint* ldq, blasint* ifst, blasint* ilst, float* work, blasint* info, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(strrfs,STRRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, float* x, blasint* ldx, float* ferr, float* berr, float* work, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(strsen,STRSEN)(char* job, char* compq, blaslogical* selectfunc, blasint* n, float* t, blasint* ldt, float* q, blasint* ldq, float* wr, float* wi, blasint* m, float* s, float* sep, float* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(strsna,STRSNA)(char* job, char* howmny, blaslogical* selectfunc, blasint* n, float* t, blasint* ldt, float* vl, blasint* ldvl, float* vr, blasint* ldvr, float* s, float* sep, blasint* mm, blasint* m, float* work, blasint* ldwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(strsyl,STRSYL)(char* trana, char* tranb, blasint* isgn, blasint* m, blasint* n, float* a, blasint* lda, float* b, blasint* ldb, float* c, blasint* ldc, float* scale, blasint* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb);

    void FC_GLOBAL(strti2,STRTI2)(char* uplo, char* diag, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(strtri,STRTRI)(char* uplo, char* diag, blasint* n, float* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(strtrs,STRTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, float* a, blasint* lda, float* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(strttf,STRTTF)(char* transr, char* uplo, blasint* n, float* a, blasint* lda, float* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(strttp,STRTTP)(char* uplo, blasint* n, float* a, blasint* lda, float* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(stzrqf,STZRQF)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, blasint* info);

    void FC_GLOBAL(stzrzf,STZRZF)(blasint* m, blasint* n, float* a, blasint* lda, float* tau, float* work, blasint* lwork, blasint* info);

    void FC_GLOBAL_(xerbla_array,XERBLA_ARRAY)(char* srname_array, blasint* srname_len, blasint* info, flexiblas_fortran_charlen_t len_srname_array);

    void FC_GLOBAL(zbbcsd,ZBBCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, blasint* m, blasint* p, blasint* q, double* theta, double* phi, lapack_complex_double* u1, blasint* ldu1, lapack_complex_double* u2, blasint* ldu2, lapack_complex_double* v1t, blasint* ldv1t, lapack_complex_double* v2t, blasint* ldv2t, double* b11d, double* b11e, double* b12d, double* b12e, double* b21d, double* b21e, double* b22d, double* b22e, double* rwork, blasint* lrwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zbdsqr,ZBDSQR)(char* uplo, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, double* d, double* e, lapack_complex_double* vt, blasint* ldvt, lapack_complex_double* u, blasint* ldu, lapack_complex_double* c, blasint* ldc, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zcgesv,ZCGESV)(blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, lapack_complex_double* work, lapack_complex_float* swork, double* rwork, blasint* iter, blasint* info);

    void FC_GLOBAL(zcposv,ZCPOSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, lapack_complex_double* work, lapack_complex_float* swork, double* rwork, blasint* iter, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zdrscl,ZDRSCL)(blasint* n, double* sa, lapack_complex_double* sx, blasint* incx);

    void FC_GLOBAL(zgbbrd,ZGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, lapack_complex_double* ab, blasint* ldab, double* d, double* e, lapack_complex_double* q, blasint* ldq, lapack_complex_double* pt, blasint* ldpt, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_vect);

    void FC_GLOBAL(zgbcon,ZGBCON)(char* norm, blasint* n, blasint* kl, blasint* ku, lapack_complex_double* ab, blasint* ldab, blasint* ipiv, double* anorm, double* rcond, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(zgbequ,ZGBEQU)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_double* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, blasint* info);

    void FC_GLOBAL(zgbequb,ZGBEQUB)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_double* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, blasint* info);

    void FC_GLOBAL(zgbrfs,ZGBRFS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* afb, blasint* ldafb, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zgbsv,ZGBSV)(blasint* n, blasint* kl, blasint* ku, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(zgbsvx,ZGBSVX)(char* fact, char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* afb, blasint* ldafb, blasint* ipiv, char* equed, double* r, double* c, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zgbtf2,ZGBTF2)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_double* ab, blasint* ldab, blasint* ipiv, blasint* info);

    void FC_GLOBAL(zgbtrf,ZGBTRF)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_double* ab, blasint* ldab, blasint* ipiv, blasint* info);

    void FC_GLOBAL(zgbtrs,ZGBTRS)(char* trans, blasint* n, blasint* kl, blasint* ku, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zgebak,ZGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, lapack_complex_double* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(zgebal,ZGEBAL)(char* job, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ilo, blasint* ihi, double* scale, blasint* info, flexiblas_fortran_charlen_t len_job);

    void FC_GLOBAL(zgebd2,ZGEBD2)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* d, double* e, lapack_complex_double* tauq, lapack_complex_double* taup, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zgebrd,ZGEBRD)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* d, double* e, lapack_complex_double* tauq, lapack_complex_double* taup, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zgecon,ZGECON)(char* norm, blasint* n, lapack_complex_double* a, blasint* lda, double* anorm, double* rcond, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(zgeequ,ZGEEQU)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* r, double* c, double* rowcnd, double* colcnd, double* amax, blasint* info);

    void FC_GLOBAL(zgeequb,ZGEEQUB)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* r, double* c, double* rowcnd, double* colcnd, double* amax, blasint* info);

    void FC_GLOBAL(zgees,ZGEES)(char* jobvs, char* sort, blaslogical* selectfunc, blasint* n, lapack_complex_double* a, blasint* lda, blasint* sdim, lapack_complex_double* w, lapack_complex_double* vs, blasint* ldvs, lapack_complex_double* work, blasint* lwork, double* rwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

    void FC_GLOBAL(zgeesx,ZGEESX)(char* jobvs, char* sort, blaslogical* selectfunc, char* sense, blasint* n, lapack_complex_double* a, blasint* lda, blasint* sdim, lapack_complex_double* w, lapack_complex_double* vs, blasint* ldvs, double* rconde, double* rcondv, lapack_complex_double* work, blasint* lwork, double* rwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(zgeev,ZGEEV)(char* jobvl, char* jobvr, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* w, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(zgeevx,ZGEEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* w, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, blasint* ilo, blasint* ihi, double* scale, double* abnrm, double* rconde, double* rcondv, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(zgegs,ZGEGS)(char* jobvsl, char* jobvsr, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* alpha, lapack_complex_double* beta, lapack_complex_double* vsl, blasint* ldvsl, lapack_complex_double* vsr, blasint* ldvsr, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr);

    void FC_GLOBAL(zgegv,ZGEGV)(char* jobvl, char* jobvr, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* alpha, lapack_complex_double* beta, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(zgehd2,ZGEHD2)(blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zgehrd,ZGEHRD)(blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zgelq2,ZGELQ2)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zgelqf,ZGELQF)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zgels,ZGELS)(char* trans, blasint* m, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zgelsd,ZGELSD)(blasint* m, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, double* s, double* rcond, blasint* rank_bn, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(zgelss,ZGELSS)(blasint* m, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, double* s, double* rcond, blasint* rank_bn, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info);

    void FC_GLOBAL(zgelsx,ZGELSX)(blasint* m, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* jpvt, double* rcond, blasint* rank_bn, lapack_complex_double* work, double* rwork, blasint* info);

    void FC_GLOBAL(zgelsy,ZGELSY)(blasint* m, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* jpvt, double* rcond, blasint* rank_bn, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info);

    void FC_GLOBAL(zgemqrt,ZGEMQRT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* nb, lapack_complex_double* v, blasint* ldv, lapack_complex_double* t, blasint* ldt, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zgeql2,ZGEQL2)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zgeqlf,ZGEQLF)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zgeqp3,ZGEQP3)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, blasint* jpvt, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info);

    void FC_GLOBAL(zgeqpf,ZGEQPF)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, blasint* jpvt, lapack_complex_double* tau, lapack_complex_double* work, double* rwork, blasint* info);

    void FC_GLOBAL(zgeqr2,ZGEQR2)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zgeqr2p,ZGEQR2P)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zgeqrf,ZGEQRF)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zgeqrfp,ZGEQRFP)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zgeqrt,ZGEQRT)(blasint* m, blasint* n, blasint* nb, lapack_complex_double* a, blasint* lda, lapack_complex_double* t, blasint* ldt, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zgeqrt2,ZGEQRT2)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(zgeqrt3,ZGEQRT3)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(zgerfs,ZGERFS)(char* trans, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* af, blasint* ldaf, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zgerq2,ZGERQ2)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zgerqf,ZGERQF)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zgesc2,ZGESC2)(blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* rhs, blasint* ipiv, blasint* jpiv, double* scale);

    void FC_GLOBAL(zgesdd,ZGESDD)(char* jobz, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* s, lapack_complex_double* u, blasint* ldu, lapack_complex_double* vt, blasint* ldvt, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobz);

    void FC_GLOBAL(zgesv,ZGESV)(blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(zgesvd,ZGESVD)(char* jobu, char* jobvt, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* s, lapack_complex_double* u, blasint* ldu, lapack_complex_double* vt, blasint* ldvt, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobvt);

    void FC_GLOBAL(zgesvx,ZGESVX)(char* fact, char* trans, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* af, blasint* ldaf, blasint* ipiv, char* equed, double* r, double* c, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zgetc2,ZGETC2)(blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, blasint* jpiv, blasint* info);

    void FC_GLOBAL(zgetf2,ZGETF2)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, blasint* info);

    void FC_GLOBAL(zgetrf,ZGETRF)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, blasint* info);

    void FC_GLOBAL(zgetri,ZGETRI)(blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zgetrs,ZGETRS)(char* trans, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zggbak,ZGGBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* lscale, double* rscale, blasint* m, lapack_complex_double* v, blasint* ldv, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(zggbal,ZGGBAL)(char* job, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* work, blasint* info, flexiblas_fortran_charlen_t len_job);

    void FC_GLOBAL(zgges,ZGGES)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* sdim, lapack_complex_double* alpha, lapack_complex_double* beta, lapack_complex_double* vsl, blasint* ldvsl, lapack_complex_double* vsr, blasint* ldvsr, lapack_complex_double* work, blasint* lwork, double* rwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort);

    void FC_GLOBAL(zggesx,ZGGESX)(char* jobvsl, char* jobvsr, char* sort, blaslogical* selctg, char* sense, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* sdim, lapack_complex_double* alpha, lapack_complex_double* beta, lapack_complex_double* vsl, blasint* ldvsl, lapack_complex_double* vsr, blasint* ldvsr, double* rconde, double* rcondv, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* iwork, blasint* liwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvsl, flexiblas_fortran_charlen_t len_jobvsr, flexiblas_fortran_charlen_t len_sort, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(zggev,ZGGEV)(char* jobvl, char* jobvr, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* alpha, lapack_complex_double* beta, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr);

    void FC_GLOBAL(zggevx,ZGGEVX)(char* balanc, char* jobvl, char* jobvr, char* sense, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* alpha, lapack_complex_double* beta, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, blasint* ilo, blasint* ihi, double* lscale, double* rscale, double* abnrm, double* bbnrm, double* rconde, double* rcondv, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* iwork, blaslogical* bwork, blasint* info, flexiblas_fortran_charlen_t len_balanc, flexiblas_fortran_charlen_t len_jobvl, flexiblas_fortran_charlen_t len_jobvr, flexiblas_fortran_charlen_t len_sense);

    void FC_GLOBAL(zggglm,ZGGGLM)(blasint* n, blasint* m, blasint* p, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* d, lapack_complex_double* x, lapack_complex_double* y, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zgghrd,ZGGHRD)(char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* q, blasint* ldq, lapack_complex_double* z, blasint* ldz, blasint* info, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(zgglse,ZGGLSE)(blasint* m, blasint* n, blasint* p, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* c, lapack_complex_double* d, lapack_complex_double* x, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zggqrf,ZGGQRF)(blasint* n, blasint* m, blasint* p, lapack_complex_double* a, blasint* lda, lapack_complex_double* taua, lapack_complex_double* b, blasint* ldb, lapack_complex_double* taub, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zggrqf,ZGGRQF)(blasint* m, blasint* p, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* taua, lapack_complex_double* b, blasint* ldb, lapack_complex_double* taub, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zggsvd,ZGGSVD)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* n, blasint* p, blasint* k, blasint* l, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, double* alpha, double* beta, lapack_complex_double* u, blasint* ldu, lapack_complex_double* v, blasint* ldv, lapack_complex_double* q, blasint* ldq, lapack_complex_double* work, double* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(zggsvp,ZGGSVP)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, lapack_complex_double* u, blasint* ldu, lapack_complex_double* v, blasint* ldv, lapack_complex_double* q, blasint* ldq, blasint* iwork, double* rwork, lapack_complex_double* tau, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(zgtcon,ZGTCON)(char* norm, blasint* n, lapack_complex_double* dl, lapack_complex_double* d, lapack_complex_double* du, lapack_complex_double* du2, blasint* ipiv, double* anorm, double* rcond, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_norm);

    void FC_GLOBAL(zgtrfs,ZGTRFS)(char* trans, blasint* n, blasint* nrhs, lapack_complex_double* dl, lapack_complex_double* d, lapack_complex_double* du, lapack_complex_double* dlf, lapack_complex_double* df, lapack_complex_double* duf, lapack_complex_double* du2, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zgtsv,ZGTSV)(blasint* n, blasint* nrhs, lapack_complex_double* dl, lapack_complex_double* d, lapack_complex_double* du, lapack_complex_double* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(zgtsvx,ZGTSVX)(char* fact, char* trans, blasint* n, blasint* nrhs, lapack_complex_double* dl, lapack_complex_double* d, lapack_complex_double* du, lapack_complex_double* dlf, lapack_complex_double* df, lapack_complex_double* duf, lapack_complex_double* du2, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zgttrf,ZGTTRF)(blasint* n, lapack_complex_double* dl, lapack_complex_double* d, lapack_complex_double* du, lapack_complex_double* du2, blasint* ipiv, blasint* info);

    void FC_GLOBAL(zgttrs,ZGTTRS)(char* trans, blasint* n, blasint* nrhs, lapack_complex_double* dl, lapack_complex_double* d, lapack_complex_double* du, lapack_complex_double* du2, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zgtts2,ZGTTS2)(blasint* itrans, blasint* n, blasint* nrhs, lapack_complex_double* dl, lapack_complex_double* d, lapack_complex_double* du, lapack_complex_double* du2, blasint* ipiv, lapack_complex_double* b, blasint* ldb);

    void FC_GLOBAL(zhbev,ZHBEV)(char* jobz, char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhbevd,ZHBEVD)(char* jobz, char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhbevx,ZHBEVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* q, blasint* ldq, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, double* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhbgst,ZHBGST)(char* vect, char* uplo, blasint* n, blasint* ka, blasint* kb, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* bb, blasint* ldbb, lapack_complex_double* x, blasint* ldx, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhbgv,ZHBGV)(char* jobz, char* uplo, blasint* n, blasint* ka, blasint* kb, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* bb, blasint* ldbb, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhbgvd,ZHBGVD)(char* jobz, char* uplo, blasint* n, blasint* ka, blasint* kb, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* bb, blasint* ldbb, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhbgvx,ZHBGVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* ka, blasint* kb, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* bb, blasint* ldbb, lapack_complex_double* q, blasint* ldq, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, double* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhbtrd,ZHBTRD)(char* vect, char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, double* d, double* e, lapack_complex_double* q, blasint* ldq, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhecon,ZHECON)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, double* anorm, double* rcond, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zheequb,ZHEEQUB)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* s, double* scond, double* amax, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zheev,ZHEEV)(char* jobz, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* w, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zheevd,ZHEEVD)(char* jobz, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* w, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zheevr,ZHEEVR)(char* jobz, char* range, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, lapack_complex_double* z, blasint* ldz, blasint* isuppz, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zheevx,ZHEEVX)(char* jobz, char* range, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhegs2,ZHEGS2)(blasint* itype, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhegst,ZHEGST)(blasint* itype, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhegv,ZHEGV)(blasint* itype, char* jobz, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, double* w, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhegvd,ZHEGVD)(blasint* itype, char* jobz, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, double* w, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhegvx,ZHEGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zherfs,ZHERFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* af, blasint* ldaf, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhesv,ZHESV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhesvx,ZHESVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* af, blasint* ldaf, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zheswapr,ZHESWAPR)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* i1, blasint* i2, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhetd2,ZHETD2)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* d, double* e, lapack_complex_double* tau, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhetf2,ZHETF2)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhetrd,ZHETRD)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* d, double* e, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhetrf,ZHETRF)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhetri,ZHETRI)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhetri2,ZHETRI2)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhetri2x,ZHETRI2X)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* nb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhetrs,ZHETRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhetrs2,ZHETRS2)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhfrk,ZHFRK)(char* transr, char* uplo, char* trans, blasint* n, blasint* k, double* alpha, lapack_complex_double* a, blasint* lda, double* beta, lapack_complex_double* c, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zhgeqz,ZHGEQZ)(char* job, char* compq, char* compz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* h, blasint* ldh, lapack_complex_double* t, blasint* ldt, lapack_complex_double* alpha, lapack_complex_double* beta, lapack_complex_double* q, blasint* ldq, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(zhpcon,ZHPCON)(char* uplo, blasint* n, lapack_complex_double* ap, blasint* ipiv, double* anorm, double* rcond, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhpev,ZHPEV)(char* jobz, char* uplo, blasint* n, lapack_complex_double* ap, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhpevd,ZHPEVD)(char* jobz, char* uplo, blasint* n, lapack_complex_double* ap, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhpevx,ZHPEVX)(char* jobz, char* range, char* uplo, blasint* n, lapack_complex_double* ap, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, double* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhpgst,ZHPGST)(blasint* itype, char* uplo, blasint* n, lapack_complex_double* ap, lapack_complex_double* bp, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhpgv,ZHPGV)(blasint* itype, char* jobz, char* uplo, blasint* n, lapack_complex_double* ap, lapack_complex_double* bp, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhpgvd,ZHPGVD)(blasint* itype, char* jobz, char* uplo, blasint* n, lapack_complex_double* ap, lapack_complex_double* bp, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhpgvx,ZHPGVX)(blasint* itype, char* jobz, char* range, char* uplo, blasint* n, lapack_complex_double* ap, lapack_complex_double* bp, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, double* rwork, blasint* iwork, blasint* ifail, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhprfs,ZHPRFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* afp, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhpsv,ZHPSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhpsvx,ZHPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* afp, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhptrd,ZHPTRD)(char* uplo, blasint* n, lapack_complex_double* ap, double* d, double* e, lapack_complex_double* tau, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhptrf,ZHPTRF)(char* uplo, blasint* n, lapack_complex_double* ap, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhptri,ZHPTRI)(char* uplo, blasint* n, lapack_complex_double* ap, blasint* ipiv, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhptrs,ZHPTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zhsein,ZHSEIN)(char* side, char* eigsrc, char* initv, blaslogical* selectfunc, blasint* n, lapack_complex_double* h, blasint* ldh, lapack_complex_double* w, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, blasint* mm, blasint* m, lapack_complex_double* work, double* rwork, blasint* ifaill, blasint* ifailr, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_eigsrc, flexiblas_fortran_charlen_t len_initv);

    void FC_GLOBAL(zhseqr,ZHSEQR)(char* job, char* compz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* h, blasint* ldh, lapack_complex_double* w, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(zlabrd,ZLABRD)(blasint* m, blasint* n, blasint* nb, lapack_complex_double* a, blasint* lda, double* d, double* e, lapack_complex_double* tauq, lapack_complex_double* taup, lapack_complex_double* x, blasint* ldx, lapack_complex_double* y, blasint* ldy);

    void FC_GLOBAL(zlacgv,ZLACGV)(blasint* n, lapack_complex_double* x, blasint* incx);

    void FC_GLOBAL(zlacn2,ZLACN2)(blasint* n, lapack_complex_double* v, lapack_complex_double* x, double* est, blasint* kase, blasint* isave);

    void FC_GLOBAL(zlacon,ZLACON)(blasint* n, lapack_complex_double* v, lapack_complex_double* x, double* est, blasint* kase);

    void FC_GLOBAL(zlacp2,ZLACP2)(char* uplo, blasint* m, blasint* n, double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zlacpy,ZLACPY)(char* uplo, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zlacrm,ZLACRM)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* b, blasint* ldb, lapack_complex_double* c, blasint* ldc, double* rwork);

    void FC_GLOBAL(zlacrt,ZLACRT)(blasint* n, lapack_complex_double* cx, blasint* incx, lapack_complex_double* cy, blasint* incy, lapack_complex_double* c, lapack_complex_double* s);

    lapack_complex_double FC_GLOBAL(zladiv,ZLADIV)(lapack_complex_double* x, lapack_complex_double* y);

    void FC_GLOBAL(zlaed0,ZLAED0)(blasint* qsiz, blasint* n, double* d, double* e, lapack_complex_double* q, blasint* ldq, lapack_complex_double* qstore, blasint* ldqs, double* rwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(zlaed7,ZLAED7)(blasint* n, blasint* cutpnt, blasint* qsiz, blasint* tlvls, blasint* curlvl, blasint* curpbm, double* d, lapack_complex_double* q, blasint* ldq, double* rho, blasint* indxq, double* qstore, blasint* qptr, blasint* prmptr, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, lapack_complex_double* work, double* rwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(zlaed8,ZLAED8)(blasint* k, blasint* n, blasint* qsiz, lapack_complex_double* q, blasint* ldq, double* d, double* rho, blasint* cutpnt, double* z, double* dlamda, lapack_complex_double* q2, blasint* ldq2, double* w, blasint* indxp, blasint* indx, blasint* indxq, blasint* perm, blasint* givptr, blasint* givcol, double* givnum, blasint* info);

    void FC_GLOBAL(zlaein,ZLAEIN)(blaslogical* rightv, blaslogical* noinit, blasint* n, lapack_complex_double* h, blasint* ldh, lapack_complex_double* w, lapack_complex_double* v, lapack_complex_double* b, blasint* ldb, double* rwork, double* eps3, double* smlnum, blasint* info);

    void FC_GLOBAL(zlaesy,ZLAESY)(lapack_complex_double* a, lapack_complex_double* b, lapack_complex_double* c, lapack_complex_double* rt1, lapack_complex_double* rt2, lapack_complex_double* evscal, lapack_complex_double* cs1, lapack_complex_double* sn1);

    void FC_GLOBAL(zlaev2,ZLAEV2)(lapack_complex_double* a, lapack_complex_double* b, lapack_complex_double* c, double* rt1, double* rt2, double* cs1, lapack_complex_double* sn1);

    void FC_GLOBAL(zlag2c,ZLAG2C)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_float* sa, blasint* ldsa, blasint* info);

    void FC_GLOBAL(zlags2,ZLAGS2)(blaslogical* upper, double* a1, lapack_complex_double* a2, double* a3, double* b1, lapack_complex_double* b2, double* b3, double* csu, lapack_complex_double* snu, double* csv, lapack_complex_double* snv, double* csq, lapack_complex_double* snq);

    void FC_GLOBAL(zlagtm,ZLAGTM)(char* trans, blasint* n, blasint* nrhs, double* alpha, lapack_complex_double* dl, lapack_complex_double* d, lapack_complex_double* du, lapack_complex_double* x, blasint* ldx, double* beta, lapack_complex_double* b, blasint* ldb, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zlahef,ZLAHEF)(char* uplo, blasint* n, blasint* nb, blasint* kb, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zlahqr,ZLAHQR)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* h, blasint* ldh, lapack_complex_double* w, blasint* iloz, blasint* ihiz, lapack_complex_double* z, blasint* ldz, blasint* info);

    void FC_GLOBAL(zlahr2,ZLAHR2)(blasint* n, blasint* k, blasint* nb, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* t, blasint* ldt, lapack_complex_double* y, blasint* ldy);

    void FC_GLOBAL(zlahrd,ZLAHRD)(blasint* n, blasint* k, blasint* nb, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* t, blasint* ldt, lapack_complex_double* y, blasint* ldy);

    void FC_GLOBAL(zlaic1,ZLAIC1)(blasint* job, blasint* j, lapack_complex_double* x, double* sest, lapack_complex_double* w, lapack_complex_double* gamma, double* sestpr, lapack_complex_double* s, lapack_complex_double* c);

    void FC_GLOBAL(zlals0,ZLALS0)(blasint* icompq, blasint* nl, blasint* nr, blasint* sqre, blasint* nrhs, lapack_complex_double* b, blasint* ldb, lapack_complex_double* bx, blasint* ldbx, blasint* perm, blasint* givptr, blasint* givcol, blasint* ldgcol, double* givnum, blasint* ldgnum, double* poles, double* difl, double* difr, double* z, blasint* k, double* c, double* s, double* rwork, blasint* info);

    void FC_GLOBAL(zlalsa,ZLALSA)(blasint* icompq, blasint* smlsiz, blasint* n, blasint* nrhs, lapack_complex_double* b, blasint* ldb, lapack_complex_double* bx, blasint* ldbx, double* u, blasint* ldu, double* vt, blasint* k, double* difl, double* difr, double* z, double* poles, blasint* givptr, blasint* givcol, blasint* ldgcol, blasint* perm, double* givnum, double* c, double* s, double* rwork, blasint* iwork, blasint* info);

    void FC_GLOBAL(zlalsd,ZLALSD)(char* uplo, blasint* smlsiz, blasint* n, blasint* nrhs, double* d, double* e, lapack_complex_double* b, blasint* ldb, double* rcond, blasint* rank_bn, lapack_complex_double* work, double* rwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(zlangb,ZLANGB)(char* norm, blasint* n, blasint* kl, blasint* ku, lapack_complex_double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(zlange,ZLANGE)(char* norm, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(zlangt,ZLANGT)(char* norm, blasint* n, lapack_complex_double* dl, lapack_complex_double* d, lapack_complex_double* du, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(zlanhb,ZLANHB)(char* norm, char* uplo, blasint* n, blasint* k, lapack_complex_double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(zlanhe,ZLANHE)(char* norm, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(zlanhf,ZLANHF)(char* norm, char* transr, char* uplo, blasint* n, lapack_complex_double* a, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(zlanhp,ZLANHP)(char* norm, char* uplo, blasint* n, lapack_complex_double* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(zlanhs,ZLANHS)(char* norm, blasint* n, lapack_complex_double* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(zlanht,ZLANHT)(char* norm, blasint* n, double* d, lapack_complex_double* e, flexiblas_fortran_charlen_t len_norm);

    double FC_GLOBAL(zlansb,ZLANSB)(char* norm, char* uplo, blasint* n, blasint* k, lapack_complex_double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(zlansp,ZLANSP)(char* norm, char* uplo, blasint* n, lapack_complex_double* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(zlansy,ZLANSY)(char* norm, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo);

    double FC_GLOBAL(zlantb,ZLANTB)(char* norm, char* uplo, char* diag, blasint* n, blasint* k, lapack_complex_double* ab, blasint* ldab, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    double FC_GLOBAL(zlantp,ZLANTP)(char* norm, char* uplo, char* diag, blasint* n, lapack_complex_double* ap, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    double FC_GLOBAL(zlantr,ZLANTR)(char* norm, char* uplo, char* diag, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* work, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(zlapll,ZLAPLL)(blasint* n, lapack_complex_double* x, blasint* incx, lapack_complex_double* y, blasint* incy, double* ssmin);

    void FC_GLOBAL(zlapmr,ZLAPMR)(blaslogical* forwrd, blasint* m, blasint* n, lapack_complex_double* x, blasint* ldx, blasint* k);

    void FC_GLOBAL(zlapmt,ZLAPMT)(blaslogical* forwrd, blasint* m, blasint* n, lapack_complex_double* x, blasint* ldx, blasint* k);

    void FC_GLOBAL(zlaqgb,ZLAQGB)(blasint* m, blasint* n, blasint* kl, blasint* ku, lapack_complex_double* ab, blasint* ldab, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zlaqge,ZLAQGE)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, double* r, double* c, double* rowcnd, double* colcnd, double* amax, char* equed, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zlaqhb,ZLAQHB)(char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, double* s, double* scond, double* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zlaqhe,ZLAQHE)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* s, double* scond, double* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zlaqhp,ZLAQHP)(char* uplo, blasint* n, lapack_complex_double* ap, double* s, double* scond, double* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zlaqp2,ZLAQP2)(blasint* m, blasint* n, blasint* offset, lapack_complex_double* a, blasint* lda, blasint* jpvt, lapack_complex_double* tau, double* vn1, double* vn2, lapack_complex_double* work);

    void FC_GLOBAL(zlaqps,ZLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, lapack_complex_double* a, blasint* lda, blasint* jpvt, lapack_complex_double* tau, double* vn1, double* vn2, lapack_complex_double* auxv, lapack_complex_double* f, blasint* ldf);

    void FC_GLOBAL(zlaqr0,ZLAQR0)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* h, blasint* ldh, lapack_complex_double* w, blasint* iloz, blasint* ihiz, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zlaqr1,ZLAQR1)(blasint* n, lapack_complex_double* h, blasint* ldh, lapack_complex_double* s1, lapack_complex_double* s2, lapack_complex_double* v);

    void FC_GLOBAL(zlaqr2,ZLAQR2)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, lapack_complex_double* h, blasint* ldh, blasint* iloz, blasint* ihiz, lapack_complex_double* z, blasint* ldz, blasint* ns, blasint* nd, lapack_complex_double* sh, lapack_complex_double* v, blasint* ldv, blasint* nh, lapack_complex_double* t, blasint* ldt, blasint* nv, lapack_complex_double* wv, blasint* ldwv, lapack_complex_double* work, blasint* lwork);

    void FC_GLOBAL(zlaqr3,ZLAQR3)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ktop, blasint* kbot, blasint* nw, lapack_complex_double* h, blasint* ldh, blasint* iloz, blasint* ihiz, lapack_complex_double* z, blasint* ldz, blasint* ns, blasint* nd, lapack_complex_double* sh, lapack_complex_double* v, blasint* ldv, blasint* nh, lapack_complex_double* t, blasint* ldt, blasint* nv, lapack_complex_double* wv, blasint* ldwv, lapack_complex_double* work, blasint* lwork);

    void FC_GLOBAL(zlaqr4,ZLAQR4)(blaslogical* wantt, blaslogical* wantz, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* h, blasint* ldh, lapack_complex_double* w, blasint* iloz, blasint* ihiz, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zlaqr5,ZLAQR5)(blaslogical* wantt, blaslogical* wantz, blasint* kacc22, blasint* n, blasint* ktop, blasint* kbot, blasint* nshfts, lapack_complex_double* s, lapack_complex_double* h, blasint* ldh, blasint* iloz, blasint* ihiz, lapack_complex_double* z, blasint* ldz, lapack_complex_double* v, blasint* ldv, lapack_complex_double* u, blasint* ldu, blasint* nv, lapack_complex_double* wv, blasint* ldwv, blasint* nh, lapack_complex_double* wh, blasint* ldwh);

    void FC_GLOBAL(zlaqsb,ZLAQSB)(char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, double* s, double* scond, double* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zlaqsp,ZLAQSP)(char* uplo, blasint* n, lapack_complex_double* ap, double* s, double* scond, double* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zlaqsy,ZLAQSY)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* s, double* scond, double* amax, char* equed, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zlar1v,ZLAR1V)(blasint* n, blasint* b1, blasint* bn, double* lambda, double* d, double* l, double* ld, double* lld, double* pivmin, double* gaptol, lapack_complex_double* z, blaslogical* wantnc, blasint* negcnt, double* ztz, double* mingma, blasint* r, blasint* isuppz, double* nrminv, double* resid, double* rqcorr, double* work);

    void FC_GLOBAL(zlar2v,ZLAR2V)(blasint* n, lapack_complex_double* x, lapack_complex_double* y, lapack_complex_double* z, blasint* incx, double* c, lapack_complex_double* s, blasint* incc);

    void FC_GLOBAL(zlarcm,ZLARCM)(blasint* m, blasint* n, double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* c, blasint* ldc, double* rwork);

    void FC_GLOBAL(zlarf,ZLARF)(char* side, blasint* m, blasint* n, lapack_complex_double* v, blasint* incv, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(zlarfb,ZLARFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, lapack_complex_double* v, blasint* ldv, lapack_complex_double* t, blasint* ldt, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(zlarfg,ZLARFG)(blasint* n, lapack_complex_double* alpha, lapack_complex_double* x, blasint* incx, lapack_complex_double* tau);

    void FC_GLOBAL(zlarfgp,ZLARFGP)(blasint* n, lapack_complex_double* alpha, lapack_complex_double* x, blasint* incx, lapack_complex_double* tau);

    void FC_GLOBAL(zlarft,ZLARFT)(char* direct, char* storev, blasint* n, blasint* k, lapack_complex_double* v, blasint* ldv, lapack_complex_double* tau, lapack_complex_double* t, blasint* ldt, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(zlarfx,ZLARFX)(char* side, blasint* m, blasint* n, lapack_complex_double* v, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(zlargv,ZLARGV)(blasint* n, lapack_complex_double* x, blasint* incx, lapack_complex_double* y, blasint* incy, double* c, blasint* incc);

    void FC_GLOBAL(zlarnv,ZLARNV)(blasint* idist, blasint* iseed, blasint* n, lapack_complex_double* x);

    void FC_GLOBAL(zlarrv,ZLARRV)(blasint* n, double* vl, double* vu, double* d, double* l, double* pivmin, blasint* isplit, blasint* m, blasint* dol, blasint* dou, double* minrgp, double* rtol1, double* rtol2, double* w, double* werr, double* wgap, blasint* iblock, blasint* indexw, double* gers, lapack_complex_double* z, blasint* ldz, blasint* isuppz, double* work, blasint* iwork, blasint* info);

    void FC_GLOBAL(zlartg,ZLARTG)(lapack_complex_double* f, lapack_complex_double* g, double* cs, lapack_complex_double* sn, lapack_complex_double* r);

    void FC_GLOBAL(zlartv,ZLARTV)(blasint* n, lapack_complex_double* x, blasint* incx, lapack_complex_double* y, blasint* incy, double* c, lapack_complex_double* s, blasint* incc);

    void FC_GLOBAL(zlarz,ZLARZ)(char* side, blasint* m, blasint* n, blasint* l, lapack_complex_double* v, blasint* incv, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(zlarzb,ZLARZB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, lapack_complex_double* v, blasint* ldv, lapack_complex_double* t, blasint* ldt, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(zlarzt,ZLARZT)(char* direct, char* storev, blasint* n, blasint* k, lapack_complex_double* v, blasint* ldv, lapack_complex_double* tau, lapack_complex_double* t, blasint* ldt, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(zlascl,ZLASCL)(char* type_bn, blasint* kl, blasint* ku, double* cfrom, double* cto, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn);

    void FC_GLOBAL(zlaset,ZLASET)(char* uplo, blasint* m, blasint* n, lapack_complex_double* alpha, lapack_complex_double* beta, lapack_complex_double* a, blasint* lda, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zlasr,ZLASR)(char* side, char* pivot, char* direct, blasint* m, blasint* n, double* c, double* s, lapack_complex_double* a, blasint* lda, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_pivot, flexiblas_fortran_charlen_t len_direct);

    void FC_GLOBAL(zlassq,ZLASSQ)(blasint* n, lapack_complex_double* x, blasint* incx, double* scale, double* sumsq);

    void FC_GLOBAL(zlaswp,ZLASWP)(blasint* n, lapack_complex_double* a, blasint* lda, blasint* k1, blasint* k2, blasint* ipiv, blasint* incx);

    void FC_GLOBAL(zlasyf,ZLASYF)(char* uplo, blasint* n, blasint* nb, blasint* kb, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* w, blasint* ldw, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zlat2c,ZLAT2C)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_float* sa, blasint* ldsa, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zlatbs,ZLATBS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(zlatdf,ZLATDF)(blasint* ijob, blasint* n, lapack_complex_double* z, blasint* ldz, lapack_complex_double* rhs, double* rdsum, double* rdscal, blasint* ipiv, blasint* jpiv);

    void FC_GLOBAL(zlatps,ZLATPS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, lapack_complex_double* ap, lapack_complex_double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(zlatrd,ZLATRD)(char* uplo, blasint* n, blasint* nb, lapack_complex_double* a, blasint* lda, double* e, lapack_complex_double* tau, lapack_complex_double* w, blasint* ldw, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zlatrs,ZLATRS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* x, double* scale, double* cnorm, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag, flexiblas_fortran_charlen_t len_normin);

    void FC_GLOBAL(zlatrz,ZLATRZ)(blasint* m, blasint* n, blasint* l, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work);

    void FC_GLOBAL(zlatzm,ZLATZM)(char* side, blasint* m, blasint* n, lapack_complex_double* v, blasint* incv, lapack_complex_double* tau, lapack_complex_double* c1, lapack_complex_double* c2, blasint* ldc, lapack_complex_double* work, flexiblas_fortran_charlen_t len_side);

    void FC_GLOBAL(zlauu2,ZLAUU2)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zlauum,ZLAUUM)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpbcon,ZPBCON)(char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, double* anorm, double* rcond, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpbequ,ZPBEQU)(char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, double* s, double* scond, double* amax, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpbrfs,ZPBRFS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* afb, blasint* ldafb, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpbstf,ZPBSTF)(char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpbsv,ZPBSV)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpbsvx,ZPBSVX)(char* fact, char* uplo, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* afb, blasint* ldafb, char* equed, double* s, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zpbtf2,ZPBTF2)(char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpbtrf,ZPBTRF)(char* uplo, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpbtrs,ZPBTRS)(char* uplo, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpftrf,ZPFTRF)(char* transr, char* uplo, blasint* n, lapack_complex_double* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpftri,ZPFTRI)(char* transr, char* uplo, blasint* n, lapack_complex_double* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpftrs,ZPFTRS)(char* transr, char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpocon,ZPOCON)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* anorm, double* rcond, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpoequ,ZPOEQU)(blasint* n, lapack_complex_double* a, blasint* lda, double* s, double* scond, double* amax, blasint* info);

    void FC_GLOBAL(zpoequb,ZPOEQUB)(blasint* n, lapack_complex_double* a, blasint* lda, double* s, double* scond, double* amax, blasint* info);

    void FC_GLOBAL(zporfs,ZPORFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* af, blasint* ldaf, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zposv,ZPOSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zposvx,ZPOSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* af, blasint* ldaf, char* equed, double* s, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zpotf2,ZPOTF2)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpotrf,ZPOTRF)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpotri,ZPOTRI)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpotrs,ZPOTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zppcon,ZPPCON)(char* uplo, blasint* n, lapack_complex_double* ap, double* anorm, double* rcond, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zppequ,ZPPEQU)(char* uplo, blasint* n, lapack_complex_double* ap, double* s, double* scond, double* amax, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpprfs,ZPPRFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* afp, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zppsv,ZPPSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zppsvx,ZPPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* afp, char* equed, double* s, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_equed);

    void FC_GLOBAL(zpptrf,ZPPTRF)(char* uplo, blasint* n, lapack_complex_double* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpptri,ZPPTRI)(char* uplo, blasint* n, lapack_complex_double* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpptrs,ZPPTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpstf2,ZPSTF2)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* piv, blasint* rank_bn, double* tol, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zpstrf,ZPSTRF)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* piv, blasint* rank_bn, double* tol, double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zptcon,ZPTCON)(blasint* n, double* d, lapack_complex_double* e, double* anorm, double* rcond, double* rwork, blasint* info);

    void FC_GLOBAL(zpteqr,ZPTEQR)(char* compz, blasint* n, double* d, double* e, lapack_complex_double* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(zptrfs,ZPTRFS)(char* uplo, blasint* n, blasint* nrhs, double* d, lapack_complex_double* e, double* df, lapack_complex_double* ef, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zptsv,ZPTSV)(blasint* n, blasint* nrhs, double* d, lapack_complex_double* e, lapack_complex_double* b, blasint* ldb, blasint* info);

    void FC_GLOBAL(zptsvx,ZPTSVX)(char* fact, blasint* n, blasint* nrhs, double* d, lapack_complex_double* e, double* df, lapack_complex_double* ef, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact);

    void FC_GLOBAL(zpttrf,ZPTTRF)(blasint* n, double* d, lapack_complex_double* e, blasint* info);

    void FC_GLOBAL(zpttrs,ZPTTRS)(char* uplo, blasint* n, blasint* nrhs, double* d, lapack_complex_double* e, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zptts2,ZPTTS2)(blasint* iuplo, blasint* n, blasint* nrhs, double* d, lapack_complex_double* e, lapack_complex_double* b, blasint* ldb);

    void FC_GLOBAL(zrot,ZROT)(blasint* n, lapack_complex_double* cx, blasint* incx, lapack_complex_double* cy, blasint* incy, double* c, lapack_complex_double* s);

    void FC_GLOBAL(zspcon,ZSPCON)(char* uplo, blasint* n, lapack_complex_double* ap, blasint* ipiv, double* anorm, double* rcond, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zspmv,ZSPMV)(char* uplo, blasint* n, lapack_complex_double* alpha, lapack_complex_double* ap, lapack_complex_double* x, blasint* incx, lapack_complex_double* beta, lapack_complex_double* y, blasint* incy, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zspr,ZSPR)(char* uplo, blasint* n, lapack_complex_double* alpha, lapack_complex_double* x, blasint* incx, lapack_complex_double* ap, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsprfs,ZSPRFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* afp, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zspsv,ZSPSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zspsvx,ZSPSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* afp, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsptrf,ZSPTRF)(char* uplo, blasint* n, lapack_complex_double* ap, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsptri,ZSPTRI)(char* uplo, blasint* n, lapack_complex_double* ap, blasint* ipiv, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsptrs,ZSPTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* ap, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zstedc,ZSTEDC)(char* compz, blasint* n, double* d, double* e, lapack_complex_double* z, blasint* ldz, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(zstegr,ZSTEGR)(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, lapack_complex_double* z, blasint* ldz, blasint* isuppz, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(zstein,ZSTEIN)(blasint* n, double* d, double* e, blasint* m, double* w, blasint* iblock, blasint* isplit, lapack_complex_double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info);

    void FC_GLOBAL(zstemr,ZSTEMR)(char* jobz, char* range, blasint* n, double* d, double* e, double* vl, double* vu, blasint* il, blasint* iu, blasint* m, double* w, lapack_complex_double* z, blasint* ldz, blasint* nzc, blasint* isuppz, blaslogical* tryrac, double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info, flexiblas_fortran_charlen_t len_jobz, flexiblas_fortran_charlen_t len_range);

    void FC_GLOBAL(zsteqr,ZSTEQR)(char* compz, blasint* n, double* d, double* e, lapack_complex_double* z, blasint* ldz, double* work, blasint* info, flexiblas_fortran_charlen_t len_compz);

    void FC_GLOBAL(zsycon,ZSYCON)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, double* anorm, double* rcond, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsyconv,ZSYCONV)(char* uplo, char* way, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_way);

    void FC_GLOBAL(zsyequb,ZSYEQUB)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, double* s, double* scond, double* amax, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsymv,ZSYMV)(char* uplo, blasint* n, lapack_complex_double* alpha, lapack_complex_double* a, blasint* lda, lapack_complex_double* x, blasint* incx, lapack_complex_double* beta, lapack_complex_double* y, blasint* incy, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsyr,ZSYR)(char* uplo, blasint* n, lapack_complex_double* alpha, lapack_complex_double* x, blasint* incx, lapack_complex_double* a, blasint* lda, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsyrfs,ZSYRFS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* af, blasint* ldaf, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsysv,ZSYSV)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsysvx,ZSYSVX)(char* fact, char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* af, blasint* ldaf, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* rcond, double* ferr, double* berr, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_fact, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsyswapr,ZSYSWAPR)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* i1, blasint* i2, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsytf2,ZSYTF2)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsytrf,ZSYTRF)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsytri,ZSYTRI)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsytri2,ZSYTRI2)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsytri2x,ZSYTRI2X)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* work, blasint* nb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsytrs,ZSYTRS)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zsytrs2,ZSYTRS2)(char* uplo, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, blasint* ipiv, lapack_complex_double* b, blasint* ldb, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ztbcon,ZTBCON)(char* norm, char* uplo, char* diag, blasint* n, blasint* kd, lapack_complex_double* ab, blasint* ldab, double* rcond, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztbrfs,ZTBRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztbtrs,ZTBTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* kd, blasint* nrhs, lapack_complex_double* ab, blasint* ldab, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztfsm,ZTFSM)(char* transr, char* side, char* uplo, char* trans, char* diag, blasint* m, blasint* n, lapack_complex_double* alpha, lapack_complex_double* a, lapack_complex_double* b, blasint* ldb, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztftri,ZTFTRI)(char* transr, char* uplo, char* diag, blasint* n, lapack_complex_double* a, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztfttp,ZTFTTP)(char* transr, char* uplo, blasint* n, lapack_complex_double* arf, lapack_complex_double* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ztfttr,ZTFTTR)(char* transr, char* uplo, blasint* n, lapack_complex_double* arf, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ztgevc,ZTGEVC)(char* side, char* howmny, blaslogical* selectfunc, blasint* n, lapack_complex_double* s, blasint* lds, lapack_complex_double* p, blasint* ldp, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, blasint* mm, blasint* m, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(ztgex2,ZTGEX2)(blaslogical* wantq, blaslogical* wantz, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* q, blasint* ldq, lapack_complex_double* z, blasint* ldz, blasint* j1, blasint* info);

    void FC_GLOBAL(ztgexc,ZTGEXC)(blaslogical* wantq, blaslogical* wantz, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* q, blasint* ldq, lapack_complex_double* z, blasint* ldz, blasint* ifst, blasint* ilst, blasint* info);

    void FC_GLOBAL(ztgsen,ZTGSEN)(blasint* ijob, blaslogical* wantq, blaslogical* wantz, blaslogical* selectfunc, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* alpha, lapack_complex_double* beta, lapack_complex_double* q, blasint* ldq, lapack_complex_double* z, blasint* ldz, blasint* m, double* pl, double* pr, double* dif, lapack_complex_double* work, blasint* lwork, blasint* iwork, blasint* liwork, blasint* info);

    void FC_GLOBAL(ztgsja,ZTGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, double* tola, double* tolb, double* alpha, double* beta, lapack_complex_double* u, blasint* ldu, lapack_complex_double* v, blasint* ldv, lapack_complex_double* q, blasint* ldq, lapack_complex_double* work, blasint* ncycle, blasint* info, flexiblas_fortran_charlen_t len_jobu, flexiblas_fortran_charlen_t len_jobv, flexiblas_fortran_charlen_t len_jobq);

    void FC_GLOBAL(ztgsna,ZTGSNA)(char* job, char* howmny, blaslogical* selectfunc, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, double* s, double* dif, blasint* mm, blasint* m, lapack_complex_double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(ztgsy2,ZTGSY2)(char* trans, blasint* ijob, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* c, blasint* ldc, lapack_complex_double* d, blasint* ldd, lapack_complex_double* e, blasint* lde, lapack_complex_double* f, blasint* ldf, double* scale, double* rdsum, double* rdscal, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(ztgsyl,ZTGSYL)(char* trans, blasint* ijob, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* c, blasint* ldc, lapack_complex_double* d, blasint* ldd, lapack_complex_double* e, blasint* lde, lapack_complex_double* f, blasint* ldf, double* scale, double* dif, lapack_complex_double* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(ztpcon,ZTPCON)(char* norm, char* uplo, char* diag, blasint* n, lapack_complex_double* ap, double* rcond, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztpmqrt,ZTPMQRT)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, blasint* nb, lapack_complex_double* v, blasint* ldv, lapack_complex_double* t, blasint* ldt, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(ztpqrt,ZTPQRT)(blasint* m, blasint* n, blasint* l, blasint* nb, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* t, blasint* ldt, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(ztpqrt2,ZTPQRT2)(blasint* m, blasint* n, blasint* l, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* t, blasint* ldt, blasint* info);

    void FC_GLOBAL(ztprfb,ZTPRFB)(char* side, char* trans, char* direct, char* storev, blasint* m, blasint* n, blasint* k, blasint* l, lapack_complex_double* v, blasint* ldv, lapack_complex_double* t, blasint* ldt, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* work, blasint* ldwork, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_direct, flexiblas_fortran_charlen_t len_storev);

    void FC_GLOBAL(ztprfs,ZTPRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztptri,ZTPTRI)(char* uplo, char* diag, blasint* n, lapack_complex_double* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztptrs,ZTPTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, lapack_complex_double* ap, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztpttf,ZTPTTF)(char* transr, char* uplo, blasint* n, lapack_complex_double* ap, lapack_complex_double* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ztpttr,ZTPTTR)(char* uplo, blasint* n, lapack_complex_double* ap, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ztrcon,ZTRCON)(char* norm, char* uplo, char* diag, blasint* n, lapack_complex_double* a, blasint* lda, double* rcond, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_norm, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztrevc,ZTREVC)(char* side, char* howmny, blaslogical* selectfunc, blasint* n, lapack_complex_double* t, blasint* ldt, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, blasint* mm, blasint* m, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(ztrexc,ZTREXC)(char* compq, blasint* n, lapack_complex_double* t, blasint* ldt, lapack_complex_double* q, blasint* ldq, blasint* ifst, blasint* ilst, blasint* info, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(ztrrfs,ZTRRFS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* x, blasint* ldx, double* ferr, double* berr, lapack_complex_double* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztrsen,ZTRSEN)(char* job, char* compq, blaslogical* selectfunc, blasint* n, lapack_complex_double* t, blasint* ldt, lapack_complex_double* q, blasint* ldq, lapack_complex_double* w, blasint* m, double* s, double* sep, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_compq);

    void FC_GLOBAL(ztrsna,ZTRSNA)(char* job, char* howmny, blaslogical* selectfunc, blasint* n, lapack_complex_double* t, blasint* ldt, lapack_complex_double* vl, blasint* ldvl, lapack_complex_double* vr, blasint* ldvr, double* s, double* sep, blasint* mm, blasint* m, lapack_complex_double* work, blasint* ldwork, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    void FC_GLOBAL(ztrsyl,ZTRSYL)(char* trana, char* tranb, blasint* isgn, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, lapack_complex_double* c, blasint* ldc, double* scale, blasint* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb);

    void FC_GLOBAL(ztrti2,ZTRTI2)(char* uplo, char* diag, blasint* n, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztrtri,ZTRTRI)(char* uplo, char* diag, blasint* n, lapack_complex_double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztrtrs,ZTRTRS)(char* uplo, char* trans, char* diag, blasint* n, blasint* nrhs, lapack_complex_double* a, blasint* lda, lapack_complex_double* b, blasint* ldb, blasint* info, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_diag);

    void FC_GLOBAL(ztrttf,ZTRTTF)(char* transr, char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ztrttp,ZTRTTP)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* ap, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(ztzrqf,ZTZRQF)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, blasint* info);

    void FC_GLOBAL(ztzrzf,ZTZRZF)(blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zunbdb,ZUNBDB)(char* trans, char* signs, blasint* m, blasint* p, blasint* q, lapack_complex_double* x11, blasint* ldx11, lapack_complex_double* x12, blasint* ldx12, lapack_complex_double* x21, blasint* ldx21, lapack_complex_double* x22, blasint* ldx22, double* theta, double* phi, lapack_complex_double* taup1, lapack_complex_double* taup2, lapack_complex_double* tauq1, lapack_complex_double* tauq2, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    void FC_GLOBAL(zuncsd,ZUNCSD)(char* jobu1, char* jobu2, char* jobv1t, char* jobv2t, char* trans, char* signs, blasint* m, blasint* p, blasint* q, lapack_complex_double* x11, blasint* ldx11, lapack_complex_double* x12, blasint* ldx12, lapack_complex_double* x21, blasint* ldx21, lapack_complex_double* x22, blasint* ldx22, double* theta, lapack_complex_double* u1, blasint* ldu1, lapack_complex_double* u2, blasint* ldu2, lapack_complex_double* v1t, blasint* ldv1t, lapack_complex_double* v2t, blasint* ldv2t, lapack_complex_double* work, blasint* lwork, double* rwork, blasint* lrwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_jobu1, flexiblas_fortran_charlen_t len_jobu2, flexiblas_fortran_charlen_t len_jobv1t, flexiblas_fortran_charlen_t len_jobv2t, flexiblas_fortran_charlen_t len_trans, flexiblas_fortran_charlen_t len_signs);

    void FC_GLOBAL(zung2l,ZUNG2L)(blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zung2r,ZUNG2R)(blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zungbr,ZUNGBR)(char* vect, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect);

    void FC_GLOBAL(zunghr,ZUNGHR)(blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zungl2,ZUNGL2)(blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zunglq,ZUNGLQ)(blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zungql,ZUNGQL)(blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zungqr,ZUNGQR)(blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zungr2,ZUNGR2)(blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* info);

    void FC_GLOBAL(zungrq,ZUNGRQ)(blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info);

    void FC_GLOBAL(zungtr,ZUNGTR)(char* uplo, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zunm2l,ZUNM2L)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunm2r,ZUNM2R)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmbr,ZUNMBR)(char* vect, char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_vect, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmhr,ZUNMHR)(char* side, char* trans, blasint* m, blasint* n, blasint* ilo, blasint* ihi, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunml2,ZUNML2)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmlq,ZUNMLQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmql,ZUNMQL)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmqr,ZUNMQR)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmr2,ZUNMR2)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmr3,ZUNMR3)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmrq,ZUNMRQ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmrz,ZUNMRZ)(char* side, char* trans, blasint* m, blasint* n, blasint* k, blasint* l, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zunmtr,ZUNMTR)(char* side, char* uplo, char* trans, blasint* m, blasint* n, lapack_complex_double* a, blasint* lda, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* lwork, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);

    void FC_GLOBAL(zupgtr,ZUPGTR)(char* uplo, blasint* n, lapack_complex_double* ap, lapack_complex_double* tau, lapack_complex_double* q, blasint* ldq, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_uplo);

    void FC_GLOBAL(zupmtr,ZUPMTR)(char* side, char* uplo, char* trans, blasint* m, blasint* n, lapack_complex_double* ap, lapack_complex_double* tau, lapack_complex_double* c, blasint* ldc, lapack_complex_double* work, blasint* info, flexiblas_fortran_charlen_t len_side, flexiblas_fortran_charlen_t len_uplo, flexiblas_fortran_charlen_t len_trans);


#ifdef __cplusplus
}
#endif
#endif
