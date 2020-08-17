(ns camera.simulation)

(defn motion-solver [m alpha k h [xn xn']]
  (let [x''
        (+ (- (* (/ alpha m) xn'))
           (- (* (/ k m) xn)))
        xnp1' (+ xn' (* h x''))
        xnp1 (+ xn (* h xnp1'))]
    [xnp1 xnp1']))
