def wavg_var(var,peso):
  unicos = master_pesos[var].unique()
  def df_wprop_valor(valor):
    if peso == "muestral":
      w_prop=(master_pesos.
      assign(prop = lambda x: x[var]== valor,
             pond=1).
      groupby(['muestra','ola']).
      apply(lambda x: 100*np.average(x['prop'],
      weights=x['pond'])).
      round(4)).reset_index()
    else: 
      w_prop=(master_pesos.
      assign(prop = lambda x: x[var]== valor).
      groupby(['muestra','ola']).
      apply(lambda x: 100*np.average(x['prop'],
      weights=x[peso])).
      round(4)).reset_index()

    w_prop.columns=['muestra','ola','prop']
    w_prop['valor']=valor
    
    if peso=="muestral":
      w_prop['ponderador']="muestral"
    else: 
      w_prop['ponderador']=peso
    return(w_prop)

  return pd.concat([df_wprop_valor(i) for i in unicos]).sort_values(by=['muestra','ola'])
 
