package com.notes;

import java.util.List;

import org.apache.commons.math3.analysis.polynomials.PolynomialFunctionLagrangeForm;

import com.google.common.collect.Lists;
import com.google.common.primitives.Doubles;

public class Notes {

	public static long getFactor(long l) {
		long  i =2;
		while (l%i != 0) {
			i++;
		}
		return i;
	}
	
	public static class Tuple<L,R> {
		public final L left;
		public final R right;
		
		public Tuple(L l,R r) {
			this.left = l;
			this.right = r;
		}
		
	}
	public static void main(String[] args) {
		List<Tuple<Long,Long>> l = Lists.newArrayList();
		long i = 41;
		int n = 0;
		while (n<4) {
			long f = getFactor(i);
			l.add(new Tuple<Long, Long>(i,f));
			n++;
			i+=41;
		}
		List<Double> x = Lists.newArrayList();
		List<Double> y = Lists.newArrayList();
		for (Tuple<Long, Long> t:l) {
			Long xa = t.left;
			Long ya = t.right;
			x.add(xa.doubleValue());
			y.add(ya.doubleValue());
		}
		PolynomialFunctionLagrangeForm poly = new PolynomialFunctionLagrangeForm(Doubles.toArray(x), Doubles.toArray(y));
		String ps = poly.toString();
		for (int z =4,ii=105;z<ii;z++) {
			double v = poly.value(z*1.0);
			long fv = Math.round(v);
			long fact = getFactor(z);
			System.out.println(String.format("%d : %d", fv,fact));
			
		}
		
	}

}
