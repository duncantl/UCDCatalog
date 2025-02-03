
# UCD Catalog

This package is for getting subject and course information from the UC Davis Catalog.


```{r}
subj = subjectCodes()
```



```{r}
eae = courseInfo("EAE")
```

```
courses = lapply(names(subj), courseInfo)
```
