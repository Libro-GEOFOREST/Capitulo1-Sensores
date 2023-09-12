## Enfrentamos los datos esperados a los datos reales
HAY QUE REVISAR ESTE EJEMPLO, LA FUNCION TIBBLE SIGUE SIN FUNCIONARME
fitData2 <- tibble(x=datos$V,fit = 1/(a_param+(b_param*x)+(c_param*(x^2))+(d_param*(x^3))))

datos$esperados <- fitData2$fit

ggplot(data = datos, aes(x = tita, y = esperados))+
  geom_point()+
  stat_smooth(method = "lm", se = FALSE)+
  labs(title = expression(theta), x = "Valores observados", y = "Valores ajustados")+
  theme_classic()
