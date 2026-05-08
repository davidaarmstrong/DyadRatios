// dyadratios_core.cpp
// R/C++ interface for the Dyad Ratios algorithm.
// Adapted from 'MCalc' by James Stimson (UNC), originally in main.cpp / MCalc.h.
// The interactive I/O and file reading of the original have been replaced by
// R-level wrappers; all estimation logic is preserved faithfully.

// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>
#include <algorithm>
#include <numeric>

using namespace Rcpp;

// ---------------------------------------------------------------------------
// Constants matching the original program
// ---------------------------------------------------------------------------
static const int    FORWARD  = 1;
static const int    BACKWARD = 2;
static const int    AVERAGE  = 3;   // row index in mood matrix
static const double NA_VAL   = 0.0; // original program uses 0 as NA sentinel
static const int    MAX_ITER = 50;

// ---------------------------------------------------------------------------
// CMat1: 1-indexed 2-D matrix (row x col), exactly as in the original code.
// ---------------------------------------------------------------------------
class CMat1 {
public:
    CMat1(int r, int c) : _rows(r), _cols(c) {
        _mat.assign(static_cast<size_t>((r + 1) * (c + 1)), 0.0);
    }
    double& operator()(int row, int col) {
        return _mat[static_cast<size_t>((row - 1) * _cols + (col - 1))];
    }
    double operator()(int row, int col) const {
        return _mat[static_cast<size_t>((row - 1) * _cols + (col - 1))];
    }
    int rows() const { return _rows; }
    int cols() const { return _cols; }
private:
    int _rows, _cols;
    std::vector<double> _mat;
};

// ---------------------------------------------------------------------------
// fStdev: population standard deviation over nperiods rows for column v.
// Skips NA_VAL entries.  Matches original fStdev() exactly.
// ---------------------------------------------------------------------------
static double fStdev(const CMat1& mat, int v, int nperiods) {
    double s = 0.0;
    int nGood = 0;
    for (int j = 1; j <= nperiods; ++j) {
        if (mat(j, v) != NA_VAL) { nGood++; s += mat(j, v); }
    }
    if (nGood < 2) return 0.0;
    double ave = s / nGood;
    s = 0.0;
    for (int j = 1; j <= nperiods; ++j) {
        if (mat(j, v) != NA_VAL) {
            double dev = mat(j, v) - ave;
            s += dev * dev;
        }
    }
    return std::sqrt(s / nGood);
}

// ---------------------------------------------------------------------------
// fbCor: Pearson correlation between forward and backward mood passes.
// Matches original fbCor() exactly.
// ---------------------------------------------------------------------------
static double fbCor(const CMat1& mood, int beginper, int endper) {
    long ncases = 0;
    double ax = 0.0, ay = 0.0;
    for (int p = beginper; p <= endper; ++p) {
        ncases++;
        ax += mood(FORWARD,  p);
        ay += mood(BACKWARD, p);
    }
    if (ncases == 0) return 0.0;
    ax /= ncases; ay /= ncases;
    double sxx = 0.0, syy = 0.0, sxy = 0.0;
    for (int p = beginper; p <= endper; ++p) {
        double xt = mood(FORWARD,  p) - ax;
        double yt = mood(BACKWARD, p) - ay;
        sxx += xt * xt; syy += yt * yt; sxy += xt * yt;
    }
    if (sxx <= 0.0 || syy <= 0.0) return 0.0;
    return sxy / std::sqrt(sxx * syy);
}

// ---------------------------------------------------------------------------
// isCorr: computes product-moment correlations of each issue with mood.
// Updates corr[] and cSign[].  Matches original isCorr() exactly.
// ---------------------------------------------------------------------------
static void isCorr(const CMat1& pIssue, const CMat1& mood,
                   std::vector<double>& corr, std::vector<float>& cSign,
                   int nVar, int beginper, int endper) {
    for (int v = 1; v <= nVar; ++v) {
        double sxx = 0, sxy = 0, syy = 0, ax = 0, ay = 0;
        long ncases = 0;
        for (int j = beginper; j <= endper; ++j) {
            if (pIssue(j, v) != NA_VAL) {
                ncases++;
                ax += pIssue(j, v);
                ay += mood(AVERAGE, j);
            }
        }
        if (ncases == 0) { corr[v] = 0.0; cSign[v] = 1.0f; continue; }
        ax /= ncases; ay /= ncases;
        for (int j = beginper; j <= endper; ++j) {
            if (pIssue(j, v) != NA_VAL) {
                double xt = pIssue(j, v) - ax;
                double yt = mood(AVERAGE,  j) - ay;
                sxx += xt * xt; syy += yt * yt; sxy += xt * yt;
            }
        }
        corr[v] = (sxx > 0.0 && syy > 0.0) ? sxy / std::sqrt(sxx * syy) : 0.0;
        cSign[v] = (corr[v] < 0.0) ? -1.0f : 1.0f;
    }
}

// ---------------------------------------------------------------------------
// eSmooth: optimises exponential smoothing alpha by golden-section / bracket.
// Faithfully translated from original eSmooth().
// ---------------------------------------------------------------------------
static double eSmooth(CMat1& mood, int fb, double /*alpha*/, int nperiods) {
    std::vector<double> X(nperiods + 1, 0.0);
    std::vector<double> VectAlpha(5, 0.0), Sum(5, 0.0), Dist(5, 0.0);

    double Lb = 0.5, Ub = 1.0, Low = Lb, High = Ub;
    double SSCrit = 0.001, DeltInc = 0.0002;
    VectAlpha[1] = Lb; VectAlpha[2] = 0.75; VectAlpha[3] = Ub;
    double SSInit = 1.0, SumSq = 0.0;

    auto doSmooth = [&](int Ap) {
        double a = VectAlpha[Ap];
        X[1] = mood(fb, 1);
        for (int L = 2; L <= nperiods; ++L)
            X[L] = a * mood(fb, L) + (1.0 - a) * X[L - 1];
        SumSq = 0.0;
        for (int L = 3; L <= nperiods; ++L) {
            double fe = mood(fb, L) - X[L - 1];
            SumSq += fe * fe;
        }
    };

    auto doSort = [&]() {
        for (int L = 1; L <= 3; ++L)
            for (int M = L + 1; M <= 3; ++M)
                if (VectAlpha[M] < VectAlpha[L]) {
                    std::swap(VectAlpha[M], VectAlpha[L]);
                    std::swap(Sum[M], Sum[L]);
                }
    };

    for (int Ap = 1; Ap <= 3; ++Ap) {
        doSmooth(Ap);
        Sum[Ap] = SumSq;
    }
    SSInit = Sum[1];

    int MinA = 1;
    bool ItExit = false;
    int It = 0;
    double OldD = VectAlpha[2], OldSS = 100.0;
    int SetD = 0;

    while (!ItExit) {
        It++;
        doSort();
        double SSR12 = Sum[1] - Sum[2];
        double SSR23 = Sum[2] - Sum[3];
        double DInc12 = VectAlpha[2] - VectAlpha[1];
        double DInc23 = VectAlpha[3] - VectAlpha[2];

        if (It >= 2) {
            if (SSR23 > 0) Low = VectAlpha[2];
            else if (SSR12 > 0) Low = VectAlpha[1];
            if (SSR12 < 0) High = VectAlpha[2];
            else if (SSR23 < 0) High = VectAlpha[3];
            if (Low > Lb)  Lb = Low;
            if (High < Ub) Ub = High;
        }

        if (DInc12 == 0.0 || DInc23 == 0.0) {
            VectAlpha[4] = 99.0;
        } else {
            SSR12 /= DInc12; SSR23 /= DInc23;
            double DdSSR = std::abs((SSR23 - SSR12) / (DInc12 + DInc23));
            if (It == 1) OldD = VectAlpha[2]; else OldD = VectAlpha[4];
            VectAlpha[4] = (DdSSR != 0.0)
                ? VectAlpha[2] + (((SSR12 + SSR23) / 2.0) / DdSSR) / 2.0
                : 99.0;
            SetD = 0;
        }

        if (VectAlpha[4] < Lb || VectAlpha[4] > Ub) {
            VectAlpha[4] = Lb + R::runif(0.0, 1.0) * (Ub - Lb);
            SetD = 1;
        }
        for (int L = 1; L <= 3; ++L)
            Dist[L] = std::abs(VectAlpha[L] - VectAlpha[4]);

        int Max = 1; double MaxD = Dist[1];
        for (int L = 2; L <= 3; ++L)
            if (Dist[L] > MaxD) { std::swap(MaxD, Dist[L]); Max = L; }

        OldSS = (It == 1) ? 100.0 : 100.0 * (SumSq / SSInit);
        VectAlpha[Max] = VectAlpha[4];
        doSmooth(Max);
        double RelSum = 100.0 * (SumSq / SSInit);
        Sum[Max] = SumSq;
        doSort();

        double ParmDif = OldD - VectAlpha[4];
        if (std::abs(OldSS - RelSum) < SSCrit &&
            VectAlpha[1] <= 0.995 && VectAlpha[3] > 0.5005) ItExit = true;
        if (std::abs(ParmDif) < DeltInc) ItExit = true;
        if (std::abs(Ub - Lb) < 0.001)  ItExit = true;

        MinA = 1; double MinSS = Sum[1];
        for (int L = 2; L <= 3; ++L)
            if (Sum[L] < MinSS) { MinSS = Sum[L]; MinA = L; }

        doSmooth(MinA);
        if (SetD == 1 && It < 51) ItExit = false;
    }
    return VectAlpha[MinA];
}

// ---------------------------------------------------------------------------
// DyadRatios: one forward or backward pass.  Matches original exactly.
// ---------------------------------------------------------------------------
static void dyadRatiosPass(CMat1& pIssue, CMat1& mood,
                            const std::vector<double>& valid,
                            const std::vector<float>& cSign,
                            int fb, int nVar, int nperiods,
                            bool smoothing, double& alpha, double& alphaF) {
    if (fb == BACKWARD) alphaF = alpha;
    if (fb == FORWARD)  { alpha = 1.0; alphaF = 1.0; }

    int startper, firstj, lastj, jprev;
    int cd;
    if (fb == FORWARD) {
        startper = 1; mood(fb, startper) = 100.0;
        firstj = 2; lastj = nperiods;
        cd = lastj - firstj + 1; jprev = 1;
    } else {
        startper = nperiods;
        mood(fb, startper) = mood(FORWARD, startper);
        firstj = nperiods - 1; lastj = 1;
        cd = firstj - lastj + 1; jprev = nperiods;
    }

    int j = firstj;
    while (cd > 0) {
        cd--;
        mood(fb, j) = 0.0;
        int everlap = 0;
        int firstj2 = (fb == FORWARD) ? 1 : j + 1;
        int lastj2  = (fb == FORWARD) ? j - 1 : nperiods;

        for (int j2 = firstj2; j2 <= lastj2; ++j2) {
            double sum = 0.0; float consum = 0.0; int overlap = 0;
            for (int v = 1; v <= nVar; ++v) {
                double xj   = pIssue(j,  v);
                double sngx2 = pIssue(j2, v);
                if (xj != NA_VAL && sngx2 != NA_VAL && sngx2 != 0.0) {
                    overlap++;
                    double ratio = xj / sngx2;
                    if (cSign[v] < 0) ratio = 1.0 / ratio;
                    sum    += valid[v] * ratio * mood(fb, j2);
                    consum += static_cast<float>(valid[v]);
                }
            }
            if (overlap > 0) {
                everlap++;
                mood(fb, j) += sum / consum;
            }
        }

        if (everlap > 0)
            mood(fb, j) = mood(fb, j) / everlap;
        else
            mood(fb, j) = mood(fb, jprev);

        jprev = j;
        if (fb == FORWARD) j++; else j--;
    }

    if (smoothing) {
        alpha = eSmooth(mood, fb, alpha, nperiods);
        if (fb == FORWARD) alphaF = alpha;
        std::vector<double> fVect(nperiods + 1);
        for (int tm = 1; tm <= nperiods; ++tm) fVect[tm] = mood(fb, tm);
        for (int tm = 2; tm <= nperiods; ++tm)
            fVect[tm] = alpha * mood(fb, tm) + (1.0 - alpha) * fVect[tm - 1];
        for (int tm = 1; tm <= nperiods; ++tm) mood(fb, tm) = fVect[tm];
    } else {
        alphaF = 1.0; alpha = 1.0;
    }
}

// ---------------------------------------------------------------------------
// residualize: removes first-dimension signal before second-pass estimation.
// Pools all non-missing (variable, period) pairs, regresses each issue series
// on the current average mood via OLS, then replaces issue values with
// sign-corrected residuals centred on 100.  Matches original Residualize().
// ---------------------------------------------------------------------------
static void residualize(CMat1& mood, CMat1& pIssue,
                        const std::vector<float>& cSign,
                        int nVar, int nperiods) {
    // count non-missing cells across all variables
    long nG = 0;
    for (int v = 1; v <= nVar; ++v)
        for (int p = 1; p <= nperiods; ++p)
            if (pIssue(p, v) != NA_VAL) ++nG;

    std::vector<float> sngY(nG + 1), sngX(nG + 1);
    nG = 0;
    for (int v = 1; v <= nVar; ++v) {
        for (int p = 1; p <= nperiods; ++p) {
            if (pIssue(p, v) != NA_VAL) {
                ++nG;
                // sign-correct issue so all series move in the same direction
                sngY[nG] = (cSign[v] > 0)
                    ? static_cast<float>(pIssue(p, v))
                    : static_cast<float>(-pIssue(p, v) + 200.0);
                sngX[nG] = static_cast<float>(mood(AVERAGE, p));
            }
        }
    }

    // OLS: regress sngY on sngX (matches original Regress())
    float sumX = 0, sumY = 0, sqX = 0, sqY = 0, xY = 0;
    for (long i = 1; i <= nG; ++i) {
        sumX += sngX[i]; sumY += sngY[i];
        sqX  += sngX[i] * sngX[i];
        sqY  += sngY[i] * sngY[i];
        xY   += sngY[i] * sngX[i];
    }
    float avX = sumX / nG, avY = sumY / nG;
    float ssqX = sqX - sumX * sumX / nG;
    float ssqY = sqY - sumY * sumY / nG;
    float crP  = xY  - sumX * sumY  / nG;
    float sdX  = std::sqrt(ssqX / nG);
    float sdY  = std::sqrt(ssqY / nG);
    float sngR    = (ssqX > 0 && ssqY > 0) ? crP / std::sqrt(ssqX * ssqY) : 0.0f;
    float sngBeta = (sdX > 0) ? sngR * sdY / sdX : 0.0f;
    float sngAlpha = avY - sngBeta * avX;

    // replace issue values with residuals re-centred on 100
    nG = 0;
    for (int v = 1; v <= nVar; ++v) {
        for (int p = 1; p <= nperiods; ++p) {
            if (pIssue(p, v) != NA_VAL) {
                ++nG;
                float resid = sngY[nG] - (sngAlpha + sngBeta * sngX[nG]);
                pIssue(p, v) = 100.0 + cSign[v] * resid;
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Main estimation function exposed to R via Rcpp.
//
// Arguments:
//   data_matrix  – NumericMatrix (nperiods × nVar), NA = 0 sentinel, 
//                  already aggregated and standardised to 100+10*z
//   nperiods     – number of time periods
//   nVar         – number of opinion variables
//   npass        – 1 or 2 latent dimensions
//   smoothing    – enable exponential smoothing
//   tola         – convergence tolerance
//   beginper     – first period to use (1-based)
//   endper       – last period to use (1-based)
//
// Returns a named List with:
//   mood         – numeric vector of final latent estimates (length nperiods)
//   mood_dim1    – first dimension (same as mood when npass==1)
//   mood_dim2    – second dimension (NA vector when npass==1)
//   loadings     – data.frame with variable loadings for each dimension
//   iterations   – data.frame of iteration history
//   alpha_F      – final forward smoothing alpha
//   alpha_B      – final backward smoothing alpha
//   eigenvalue   – eigenvalue estimate
//   variance_explained – proportion of variance explained
// ---------------------------------------------------------------------------
// [[Rcpp::export]]
List dyad_ratios_cpp(NumericMatrix data_matrix,
                     int nperiods, int nVar,
                     int npass, bool smoothing,
                     double tola_in,
                     int beginper, int endper,
                     NumericVector raw_av,
                     NumericVector raw_std) {

    // --- build pIssue (1-indexed CMat1) from R matrix (0-indexed) ----------
    CMat1 pIssue(nperiods + 1, nVar + 1);
    for (int p = 1; p <= nperiods; ++p)
        for (int v = 1; v <= nVar; ++v)
            pIssue(p, v) = data_matrix(p - 1, v - 1);   // R is 0-indexed

    // mood matrix rows: 1=FORWARD, 2=BACKWARD, 3=AVERAGE
    CMat1 mood(4, nperiods + 1);
    // separate storage for dim1 result (avoids out-of-bounds row-0 access)
    std::vector<double> mood_dim1(nperiods + 1, 0.0);

    // working vectors (1-indexed via offset)
    std::vector<double> corr(nVar + 1, 0.0);
    std::vector<double> valid(nVar + 1, 1.0);
    std::vector<float>  cSign(nVar + 1, 1.0f);
    std::vector<double> oldr(nVar + 1, 1.0);
    std::vector<double> r1(nVar + 1, 0.0);
    std::vector<double> av(nVar + 1, 0.0);
    std::vector<double> varStd(nVar + 1, 0.0);
    std::vector<int>    varCases(nVar + 1, 0);

    // --- per-variable means and std for metric rescaling ------------------
    // Use pre-standardisation moments passed in from R so the final mood
    // is returned in the original issue metric, not the 100+10z scale.
    double wtmean = 0.0, wtstd = 0.0;
    for (int v = 1; v <= nVar; ++v) {
        int nGood = 0;
        for (int p = beginper; p <= endper; ++p)
            if (pIssue(p, v) != NA_VAL) ++nGood;
        varCases[v] = nGood;
        av[v]     = raw_av[v - 1];   // 0-indexed R vectors
        varStd[v] = raw_std[v - 1];
    }

    // iteration history storage
    struct IterRec { int iter; double converge, tola, rely, alphaF, alphaB; };
    std::vector<IterRec> iterHistory;

    // R-dimension loadings: R[pass][v], pass 1..2
    std::vector<std::vector<double>> R_load(3, std::vector<double>(nVar + 1, 0.0));

    double evalue = 0.0, totalvar = 0.0, expprop = 0.0, tot1 = 0.0;
    double vsum = 0.0;
    double alpha = 1.0, alphaF = 1.0;
    double holdtola = tola_in;
    double lastconv = 99.0;

    // -----------------------------------------------------------------------
    for (int pass = 1; pass <= npass; ++pass) {
        double tola = holdtola;
        double converge = 0.0;
        int iter = 0;
        bool quit = false;
        lastconv = 99.0;
        alpha = 1.0; alphaF = 1.0;

        if (pass == 2) {
            // save rescaled dim1 before pass-2 overwrites mood(AVERAGE, p)
            for (int p = 1; p <= nperiods; ++p)
                mood_dim1[p] = mood(AVERAGE, p);
            residualize(mood, pIssue, cSign, nVar, nperiods);
        }

        if (pass == 1) {
            // initialise valid and cSign
            for (int v = 1; v <= nVar; ++v) {
                valid[v] = 1.0; cSign[v] = 1.0f; oldr[v] = 1.0;
            }
        } else {
            for (int v = 1; v <= nVar; ++v) { valid[v] = 1.0; }
        }

        while (iter == 0 || converge > tola) {
            // forward and backward passes
            for (int fb = 1; fb <= 2; ++fb)
                dyadRatiosPass(pIssue, mood, valid, cSign,
                               fb, nVar, nperiods, smoothing, alpha, alphaF);

            // compute average mood
            for (int p = beginper; p <= endper; ++p)
                mood(AVERAGE, p) = (mood(1, p) + mood(2, p)) / 2.0;

            iter++;
            isCorr(pIssue, mood, corr, cSign, nVar, beginper, endper);

            converge = 0.0; wtmean = 0.0; wtstd = 0.0; vsum = 0.0;
            evalue = 0.0; totalvar = 0.0;

            for (int v = 1; v <= nVar; ++v) {
                int wn = 0;
                for (int p = beginper; p <= endper; ++p)
                    if (pIssue(p, v) != NA_VAL) wn++;
                double vratio = (nperiods > 0) ? double(wn) / nperiods : 0.0;
                evalue   += vratio * corr[v] * corr[v];
                totalvar += vratio;

                if (wn > 3) {
                    double conv = std::abs(corr[v] - oldr[v]) * vratio;
                    if (conv > converge) converge = conv;
                }
                oldr[v]  = corr[v];
                valid[v] = corr[v] * corr[v];
                wtmean  += av[v]     * valid[v];
                wtstd   += varStd[v] * valid[v];
                vsum    += valid[v];
            }
            if (vsum > 0.0) { wtmean /= vsum; wtstd /= vsum; }

            double fb_corr = fbCor(mood, beginper, endper);
            if (converge > lastconv) tola *= 2.0;
            lastconv = converge;

            iterHistory.push_back({iter, converge, tola, fb_corr, alphaF, alpha});
            if (iter > MAX_ITER) break;
        } // while converge

        // final isCorr and loadings
        isCorr(pIssue, mood, corr, cSign, nVar, beginper, endper);
        for (int v = 1; v <= nVar; ++v) {
            R_load[pass][v] = corr[v];
            if (pass == 1) r1[v] = corr[v];
        }

        if (pass == 1) { expprop = evalue / totalvar; tot1 = totalvar; }

        // rescale mood to match weighted mean/std of the issues
        double msum = 0.0, msq = 0.0;
        for (int p = 1; p <= nperiods; ++p) {
            double mp = mood(AVERAGE, p);
            msum += mp; msq += mp * mp;
        }
        double moodMean = msum / nperiods;
        double sdMood   = std::sqrt(msq / nperiods - moodMean * moodMean);
        if (sdMood > 0.0) {
            for (int p = 1; p <= nperiods; ++p)
                mood(AVERAGE, p) =
                    ((mood(AVERAGE, p) - moodMean) * wtstd / sdMood) + wtmean;
        }

        if (pass < npass)
            for (int v = 1; v <= nVar; ++v) valid[v] = 1.0;
    } // pass loop

    // -----------------------------------------------------------------------
    // Package results for R
    // -----------------------------------------------------------------------
    NumericVector mood_out(nperiods);
    NumericVector mood_d1(nperiods), mood_d2(nperiods, NA_REAL);

    for (int p = 1; p <= nperiods; ++p) {
        mood_out[p - 1] = (npass == 2) ? mood_dim1[p] : mood(AVERAGE, p);
        mood_d1[p - 1]  = (npass == 2) ? mood_dim1[p] : mood(AVERAGE, p);
        if (npass == 2) mood_d2[p - 1] = mood(AVERAGE, p);
    }

    // loadings data frame
    NumericVector dim1(nVar), dim2(nVar);
    for (int v = 1; v <= nVar; ++v) {
        dim1[v - 1] = R_load[1][v];
        dim2[v - 1] = (npass == 2) ? R_load[2][v] : NA_REAL;
    }
    DataFrame loadings = DataFrame::create(
        Named("dim1") = dim1,
        Named("dim2") = dim2
    );

    // iteration history data frame
    int nh = iterHistory.size();
    IntegerVector  it_iter(nh);
    NumericVector  it_conv(nh), it_tola(nh), it_rely(nh),
                   it_alphaF(nh), it_alphaB(nh);
    for (int i = 0; i < nh; ++i) {
        it_iter[i]   = iterHistory[i].iter;
        it_conv[i]   = iterHistory[i].converge;
        it_tola[i]   = iterHistory[i].tola;
        it_rely[i]   = iterHistory[i].rely;
        it_alphaF[i] = iterHistory[i].alphaF;
        it_alphaB[i] = iterHistory[i].alphaB;
    }
    DataFrame idf = DataFrame::create(
        Named("iter")        = it_iter,
        Named("converge")    = it_conv,
        Named("criterion")   = it_tola,
        Named("reliability") = it_rely,
        Named("alpha_F")     = it_alphaF,
        Named("alpha_B")     = it_alphaB
    );

    return List::create(
        Named("mood")               = mood_out,
        Named("mood_dim1")          = mood_d1,
        Named("mood_dim2")          = mood_d2,
        Named("loadings")           = loadings,
        Named("iterations")         = idf,
        Named("alpha_F")            = alphaF,
        Named("alpha_B")            = alpha,
        Named("eigenvalue")         = evalue,
        Named("variance_explained") = (tot1 > 0) ? expprop : NA_REAL
    );
}
