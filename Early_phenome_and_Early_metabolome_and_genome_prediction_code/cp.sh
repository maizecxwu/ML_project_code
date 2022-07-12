for i in {1..30};do
sed -e '105s/1/'${i}'/g' select_var_and_pred_rf_bartMachine_added_late_Trait_bins.r \
> select_var_and_pred_rf_bartMachine_added_late_Trait_bins_${i}.r
wait
bsub -J select_var_and_pred_rf_bartMachine_added_late_Trait_bins_${i} \
-e select_var_and_pred_rf_bartMachine_added_late_Trait_bins_${i}.err \
-o select_var_and_pred_rf_bartMachine_added_late_Trait_bins_${i}.out -R span[hosts=1] -n 5 \
-q high "module load R/4.0.0;Rscript select_var_and_pred_rf_bartMachine_added_late_Trait_bins_${i}.r"
done
