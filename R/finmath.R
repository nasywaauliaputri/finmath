#' Menghitung Compound Interest
#' @description
#' Fungsi untuk menghitung jumlah akhir dan bunga berdasarkan bunga majemuk.
#' @export
#' @param principal Jumlah pokok awal.
#' @param rate Tingkat bunga per periode (dalam desimal)
#' @param time Lama waktu
#' @param comp_per_year Frekuensi kapitalisasi bunga dalam setahun (default = 1).
#' @return Sebuah list berisi amount(total nilai setelah bunga majemuk dihitung) dan interest(jumlah bunga yang diperoleh selama periode).
# File: R/compound_interest.R
compound_interest <- function(principal, rate, time, comp_per_year = 1) {
  # Menghitung jumlah total dengan bunga majemuk
  amount <- principal * (1 + rate / comp_per_year)^(comp_per_year * time)
  interest <- amount - principal

  return(list(amount = amount, interest = interest))
}

#' Menghitung Present Value
#' @description
#' Menghitung nilai sekarang berdasarkan nilai masa depan, bunga, dan waktu.
#' @export
#' @param future_value Nilai masa depan (future value)
#' @param rate Tingkat bunga per periode (dalam desimal)
#' @param time Lama waktu
#' @return Nilai sekarang
# File: R/present_value.R
present_value <- function(future_value, rate, time) {
  # Menghitung nilai sekarang
  present_value <- future_value / (1 + rate)^time

  return(list(present_value = present_value))
}

#' Menghitung Future Value
#' @description
#' Menghitung nilai masa depan berdasarkan pokok awal, bunga, dan waktu.
#' @export
#' @param principal Jumlah pokok awal
#' @param rate Tingkat bunga per periode (dalam desimal)
#' @param time Lama waktu
#' @return Nilai masa depan
# File: R/future_value.R
future_value <- function(principal, rate, time) {
  # Menghitung nilai masa depan
  future_value <- principal * (1 + rate)^time

  return(list(future_value = future_value))
}


#' Menghitung Effective Discount
#' @description
#' Menghitung tingkat diskon efektif
#' @export
#' @param rate Tingkat bunga (dalam desimal)
#' @return Diskon efektif
#File: R/effective_discount.R
effective_discount <- function(rate) {
  effective_discount = rate / (1 + rate)

  return(list(effective_discount = effective_discount))
}

#' Menghitung Annuity Immediate
#' @description
#' Menghitung nilai sekarang dari anuitas yang pembayarannya dilakukan di setiap akhir periode.
#' @export
#' @param payment Pembayaran per periode
#' @param rate Tingkat bunga per periode
#' @param n Jumlah periode
#' @return Nilai Annuity Immediate, yaitu total nilai dari serangkaian pembayaran dengan bunga yang dibayarkan di setiap akhir periode.
#File: R/annuity_immediate.R
annuity_immediate <- function(payment, rate, n) {
  annuity_immediate = payment * ((1 - (1 + rate)^(-n)) / rate)
  return(list(annuity_immediate = annuity_immediate))
}

#' Menghitung Annuity Due
#' @description
#' Fungsi ini menghitung nilai Annuity Due, yaitu serangkaian pembayaran yang dilakukan di awal setiap periode.
#' @param payment Pembayaran yang dilakukan setiap periode.
#' @param rate Tingkat bunga per periode (dalam desimal).
#' @param n Jumlah periode pembayaran.
#' @return Nilai Annuity Due, yaitu total nilai dari serangkaian pembayaran dengan bunga yang dibayarkan di setiap awal periode.
#' @export
#File: R/annuity_due.R
annuity_due <- function(payment, rate, n) {
  annuity_due = payment * ((1 - (1 + rate)^(-n)) / rate) * (1 + rate)
  return(list(annuity_due = annuity_due))
}

#' Menghitung Perpetuity
#' @description
#' Menghitung nilai perpetuitas
#' @param payment Pembayaran per periode
#' @param rate Tingkat bunga per periode
#' @return Nilai perpetuitas
#' @export
#File: R/perpetuity.R
perpetuity <- function(payment, rate) {
  perpetuity = payment / rate

  return(list(perpetuity = perpetuity))
}

#' Menghitung Loan Payment
#' @description
#' Menghitung pembayaran pinjaman secara berkala.
#' @param principal Jumlah pokok pinjaman
#' @param rate Tingkat bunga per periode (dalam desimal)
#' @param n Jumlah periode
#' @return Pembayaran pinjaman per periode
#' @export
#File: R/loan_payment.R
loan_payment <- function(principal, rate, n) {
  loan_payment = principal * rate / (1 - (1 + rate)^(-n))

  return(list(loan_payment = loan_payment))
}



#' Menghitung Remaining Balance
#' @description Menghitung sisa saldo pinjaman setelah beberapa pembayaran.
#' @param principal Jumlah pokok pinjaman.
#' @param rate Tingkat bunga per periode (dalam desimal).
#' @param n Jumlah total periode.
#' @param t Jumlah periode yang telah dilalui.
#' @return Sisa saldo pinjaman.
#' @export
#File: R/remaining_balance.R
remaining_balance <- function(principal, rate, n, t) {
  remaining_balance = principal * ((1 + rate)^n - (1 + rate)^t) / ((1 + rate)^n - 1)

  return(list(remaining_balance = remaining_balance))

}

#' Menghitung Sinking Fund
#' @description Menghitung pembayaran berkala untuk sinking fund.
#' @param FV Nilai masa depan (future value) yang diinginkan.
#' @param rate Tingkat bunga per periode (dalam desimal).
#' @param m Frekuensi pembayaran dalam setahun.
#' @param n Jumlah tahun.
#' @return Pembayaran berkala untuk sinking fund.
#' @export
#File: R/sinking_fund.R
sinking_fund <- function(FV, rate, m, n) {
  sinking_fund = ((1 + rate/m)^(n * m) - 1) / (rate/m) * FV

  return(list(sinking_fund = sinking_fund))
}
