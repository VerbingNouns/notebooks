
fix_align <- function(start_pts, 
											asc_files='.',
											xy_bounds=NULL, 
											keep_y_var=FALSE,
											use_run_rule=TRUE,
											trial_plots=TRUE,
											save_trial_plots=FALSE,
											summary_file=TRUE,
											show_image=FALSE,
											fa_dir='FA_Dir',
											start_flag='TRIALID',
										  den_sd_cutoff=Inf, 
											den_ratio_cutoff=1,
											k_bounds=c(-.1, .1), 
											o_bounds=c(-50, 50), 
											s_bounds=c(1, 20)) {

	# start_pts: n (number lines) x 2 matrix of c(x, y) for start point of each line 
	# asc_files: A vector of folders containing asc files and asc file names
	# xy_bounds: c(x_min, x_max, y_min, y_max) or 
	#		N (number trials) x 4 (x_min, x_max, y_min, y_max) matrix 
	#		for which points to keep, if NULL, no bounds. 
	#	keep_y_var: Keep variability in y around the line
	# use_run_rule: If a series of ambiguous pts are bounded by points in the same 
	#		category, classify the ambiguous points into that category.
	# trial_plots: Show trial-by-trial plots.
	# save_trial_plots: Save the trial plots as a tiff?
	# summary_file: Output summary file.
	# show_image: Show background images on trial plots. 
	#		Images are tiffs in same directory as data (<sub>.asc) 
	#		named <sub>.tiff (1 per subject) or <sub>_<trial#>.tiff (1 per subject per trial).
	# fa_dir: Where to store the reformatted asc files
	# start_flag: what text starts the trial in the asc file?
	# den_sd_cutoff: remove points for which the density is > this many sd away 
	#		from mean density
	# den_ratio_cutoff: remove points for which (max density)/(2nd max density) 
	#		not high enough
	# k_bounds: c(min, max) for line slope, if NULL, set to default (0)
	# o_bounds: c(min, max) for vertical line offset, if NULL, set to default (0)
	# s_bounds: c(min, max) for line sd
	
	# In fa.asc file
	# 	-1000 = not in trial ('nit', fixation ends before start_flag)
	# 	-1001 = partial fixation ('part', fixation started before start_flag, but ends after)
	#		-1002 = out-of-bounds fixation ('oob')
	#		-1003 = ambiguous fixation ('amb')
	#		-1004 = low-density fixation ('den')

	# Andrew L. Cohen
	# July 19, 2012
	
	# © 2012
	#	University of Massachusetts
	# All Rights Reserved

	# fix_align.R v0.91

	# Version notes:
	#		0.90 - Original
	#		0.91 - 11/28/12 alc
	#					 Handles situations where two trials starts are not separated by a trial end or 
	#					 two fixation starts are not separated by a fixation end.
  #   0.92 - 2/21/14 alc
  #          Inserts windows line breaks in the output file.

	# Determine the names of the data files
	files <- get_file_names(asc_files)

	# Name of output file based on time and date
	# fas: (f)ix_(a)lign (s)ummary
	if (summary_file) {

		out_file_name <-  
			paste(fa_dir, '/', gsub('\\s+', '_', format(Sys.time(), format='%B %d %Y %T')), '.fas', sep='')
		out_file <- file(out_file_name, 'w')	
		cat('File Trial Slope VOffset SD Fit n_total_fix n_keep n_oob n_amb n_den n_nit n_part\n',
				file=out_file)

	}

	# The number of files to process
	n_files <- length(files)

	# Go through the files
	for (i in 1:n_files) {

		# To keep track of progress
		print(paste('Processing: ', files[i], sep=""))

		# Read in the asc_file
		asc_data <- read_asc_file(files[i], start_flag)

		# Initialize list
		line_ret <- list()

		# Go through the trials
		n_trials <- asc_data$n_trials
		for (t in 1:n_trials) {

			# Get the data for this trial
			fix_data_trial <- asc_data$fix_data[[t]]		

			# Mark out-of-bound fixations	
			fix_data_trial <- mark_out_of_bounds(fix_data_trial, xy_bounds, t, n_trials)

			# Find the best-fitting lines
			line_ret[[t]] <- fit_lines(start_pts, 
														 		 fix_data_trial, 
																 keep_y_var,
																 use_run_rule,
													  		 k_bounds, o_bounds, s_bounds, 
													  		 den_sd_cutoff, den_ratio_cutoff)

			# Trial plots
			if (trial_plots) 
				do_trial_plots(line_ret[[t]], t, files[i], asc_data$dur_summary, 
									     show_image, save_trial_plots, fa_dir)				

		}

		# Write the updated asc_file
		write_asc_file(line_ret, files[i], n_trials, fa_dir, start_flag, asc_data)

		# Save summary	
		if (summary_file)
			save_summary(line_ret, files[i], out_file, n_trials)

	}

	# Close the summary file
	if (summary_file)
		close(out_file)
  
}

##############################
# get_file_names
##############################
get_file_names <- function(asc_files) {

	# The number of entries in the vector
	n_entries <- length(asc_files) 

	# Initialize variables
	files <- vector(mode='character')

	# Go through each entry
	for (i in 1:n_entries) {

		# Get an asc entry
		asc_entry <- asc_files[i]

		# Determine if it is a directory or vector of files
		is_dir <- file.info(asc_entry)$isdir

		# If it doesn't exist, change to false, it could be missing '.asc'
		if (is.na(is_dir)) 
			is_dir <- FALSE

		# Folders
		if (is_dir) {

			# Make sure there is a / at the end of the folder name
			if (substr(asc_entry, nchar(asc_entry), nchar(asc_entry)) != '/')
				asc_entry <- paste(asc_entry, '/', sep='')				

			files <- append(files, paste(asc_entry, 
														       dir(path=asc_entry, pattern='*.asc'), 
																   sep=''))
	
		# File or doesn't exist
		}	else {

			# Delete and paste '.asc' to the end
			asc_entry <- paste(sub('.asc', '', asc_entry, fixed=TRUE), '.asc', sep='')

			# Determine if the file exists
			file_exists <- file.exists(asc_entry)			

			# Not found
			if (file_exists)
				files <- append(files, asc_entry)

			else
				warning('ASC file/folder ', asc_entry, ' not found.')

		}

	}

	# If no valid entries found
	if (length(files) == 0)
		stop('No valid ASC files/folders found.')

	# Get rid of duplicates
	files <- unique(files)

	# Return the vector of files to process
	return(files)

} 

##############################
# read_asc_file
##############################
read_asc_file <- function(asc_file_name, start_flag) {

	# Constants
	col_range <- 5:8

	# Read in the file
	f <- readLines(asc_file_name)

	# Get trial start and end
	trial_id_start <- which(grepl('TRIALID', f))
	trial_start <- which(grepl(start_flag, f))
	trial_end <- which(grepl('TRIAL_RESULT', f))

	# Handle situations where two trials starts are not 
	# separated by trial ends (e.g., due to aborted trials).
	# Get rid of first one.	

	# Get rid of errant TRIALIDs
	marked_starts <- numeric()
	start_index <- 1
	end_index <- 1
	while (start_index < length(trial_id_start)) {
		if (trial_id_start[start_index + 1] < trial_end[end_index]) {
			marked_starts <- c(marked_starts, start_index)
		} else {
			end_index <- end_index + 1
		}
		start_index <- start_index + 1
	}

	if (length(marked_starts) > 0) 
		trial_id_start <- trial_id_start[-marked_starts]	

	# Get rid of errant start_flags
	marked_flags <- numeric()
	flag_index <- 1
	end_index <- 1
	while (flag_index < length(trial_id_start)) {
		if (trial_start[flag_index + 1] < trial_end[end_index]) {
			marked_flags <- c(marked_flags, flag_index)
		} else {
			end_index <- end_index + 1
		}
		flag_index <- flag_index + 1
	}

	if (length(marked_flags) > 0) 
		trial_start <- trial_start[-marked_flags]	

	# Number of trials
	n_trials <- length(trial_start)

	# Initialize fixation data
	fix_data <- list() 

	# Keep duration data in a vector to get summary info
	all_dur <- numeric(0)

	# Analyze each trial
	for (t in 1:n_trials) {

		# Data for one trial
		trial_data  <- f[trial_id_start[t]:trial_end[t]]

		# Find the start and end of the fixations
		trial_start_fix <- which(grepl('SFIX', trial_data))
		trial_end_fix <- which(grepl('EFIX', trial_data))

		# Handle situations where two start fixations are not 
		# separated by end fixations (e.g., due to track loss).
		# Get rid of first one.	
		marked_starts <- numeric()
		start_index <- 1
		end_index <- 1
		while (start_index < length(trial_start_fix)) {
			if (trial_start_fix[start_index + 1] < trial_end_fix[end_index]) {
				marked_starts <- c(marked_starts, start_index)
			} else {
				end_index <- end_index + 1
			}
			start_index <- start_index + 1
		}

		trial_start_fix <- trial_start_fix[-marked_starts]				

		# Fixations for a trial and get rid of extra white space
		trial_fix <- trial_data[trial_end_fix]
		trial_fix <- gsub('\\s+', ' ', trial_fix)	
		trial_fix <- strsplit(trial_fix, ' ')
	
		# Fixation duration, location, pupil size
		n_fix <- length(trial_fix)

		# n_fixations x 4 (duration, x, y, pupil_size)
		fix_data_trial <- matrix(NA, nrow=n_fix, ncol=4)

		for (i in 1:n_fix) {
			fix_data_trial[i,1:4] <- as.numeric(trial_fix[[i]][col_range])
		}

		fix_data_trial <- data.frame(fix_data_trial)
		names(fix_data_trial) <- c('dur', 'x', 'y', 'pupil')

		# Originally mark all fixations as keepers
		fix_data_trial$type = 'keep'

		# Mark any fixations before the start flag as not in trial (NIT)
		fix_data_trial$type[trial_end_fix < (trial_start[t] - trial_id_start[t] + 1)] <- 'nit'

		# Find the first full fixation after the start flag, mark others as partial (part)
		fix_data_trial$type[trial_start_fix < (trial_start[t] - trial_id_start[t] + 1) & 
											  trial_end_fix > (trial_start[t] - trial_id_start[t] + 1)] <- 'part'

		# Store duration information
		all_dur <- append(all_dur, fix_data_trial$dur)

		fix_data[[t]] <- fix_data_trial

	} 

	# Put all data together in one list
	asc_data <- list()

	asc_data$fix_data <- fix_data
	asc_data$n_trials <- n_trials
	asc_data$dur_summary <- fivenum(all_dur)

	asc_data$trial_id_start <- trial_id_start
	asc_data$trial_start <- trial_start
	asc_data$trial_end <- trial_end

	return(asc_data)

}

##############################
# mark_out_of_bounds
##############################
mark_out_of_bounds <- function(fix_data_trial, xy_bounds, t, n_trials) {

	# If we use bounds
	if (!is.null(xy_bounds)) {

		# Bounds for this trial
		if (is.null(nrow(xy_bounds)) || nrow(xy_bounds) == 1)

			xy_bounds_trial <- xy_bounds

		else {

			# Make sure there are enough xy_bounds entries
			if (nrow(xy_bounds) != n_trials)
				stop('Length of xy_bounds doesn\'t match number of trials.')
			else
				xy_bounds_trial <- xy_bounds[t,]

		}

		# Mark out-of-bounds fixations	
		x_keepers <- fix_data_trial$x>xy_bounds_trial[1] & fix_data_trial$x<xy_bounds_trial[2]
		y_keepers <- fix_data_trial$y>xy_bounds_trial[3] & fix_data_trial$y<xy_bounds_trial[4]

		# Give partial & not in trial fixations priority for marking
		nit <- fix_data_trial$type == 'nit'
		part <- fix_data_trial$type == 'part'

		# Mark as out-of-bounds 
		fix_data_trial$type[!(x_keepers & y_keepers) & !part & !nit] <- 'oob'

	}

	# Return it
	return(fix_data_trial)

}

##############################
# fit_lines
##############################
fit_lines <- function(start_pts, 
							        fix_data_trial, 
											keep_y_var,					
											use_run_rule,					
											k_bounds, o_bounds, s_bounds, 
											den_sd_cutoff, den_ratio_cutoff) {

	# Intitial parameter values (slope, vertical offset, sd)
	init_params = c(0, 0, 0)

	# Find the best fitting parameters
	fit <- optim(init_params, 
							 create_lines, 
			         fix_data=fix_data_trial, 
						   start_pts=start_pts, 
							 k_bounds=k_bounds, o_bounds=o_bounds, s_bounds=s_bounds)

	# Rerun to get additional information
	line_ret <- create_lines(fit$par, 
		  										 fit_it=FALSE, 
			 									   start_pts=start_pts, 
					                 fix_data=fix_data_trial, 
													 keep_y_var=keep_y_var,
													 use_run_rule=use_run_rule,
													 den_sd_cutoff=den_sd_cutoff, 
					                 den_ratio_cutoff=den_ratio_cutoff, 
													 k_bounds=k_bounds, o_bounds=o_bounds, s_bounds=s_bounds)

	# Return
	return(line_ret)

}

##############################
# create_lines
##############################
create_lines <- function(params, 
										     fix_data,
										     start_pts, 
									    	 k_bounds, o_bounds, s_bounds,
										  	 fit_it=TRUE, 
												 keep_y_var,
												 use_run_rule,
										  	 den_sd_cutoff, den_ratio_cutoff) {

	# fit_it: TRUE -> return fit measure, FALSE -> return fit information
	# den_sd_cutoff: remove points for which the density is > this many sd away from mean density
	# den_ratio_cutoff: remove points for which (max density)/(2nd max density) not high enough

	# Unpack the parameters
	if (is.null(k_bounds))
		k <- 0
	else
		k <- k_bounds[1] + (k_bounds[2] - k_bounds[1])*pnorm(params[1])

	if (is.null(o_bounds))
		o <- 0
	else
		o <- o_bounds[1] + (o_bounds[2] - o_bounds[1])*pnorm(params[2])

	s <- s_bounds[1] + (s_bounds[2] - s_bounds[1])*pnorm(params[3])

  # The y-values for the lines
  ys <- start_pts[,2]

	# The number of clusters is based off of the lines
	n_clusters <- length(ys)

	# Initialize some matrices
	data_den <- matrix(numeric(0), nrow(fix_data), n_clusters)
	y_diff <- matrix(numeric(0), nrow(fix_data), n_clusters)

	for (l in 1:n_clusters) {

		# The value of each point on each line
		y_on_line <- o + k*(fix_data$x - start_pts[l,1]) + start_pts[l,2]

		# Log density value for each point based on the line and sd
		data_den[,l] <- log(dnorm(fix_data$y, mean=y_on_line, sd=s))

		# Store the difference between the real and fitted value
		y_diff[,l] <- fix_data$y - y_on_line

	}

	# Find max density line for each point
	# Assume all-or-none classification
	data_den_max <- apply(data_den, 1, max)
	
	# The sum of the log densitities is the fit measure
	# Only use valid, in-bounds fixations

	fit_measure <- -sum(data_den_max[fix_data$type == 'keep'])

	if (fit_it) {
		
		# In case log density goes to infinity
		if (fit_measure == Inf) fit_measure = .Machine$integer.max

		# Return the fit measure
		return(fit_measure)

	} else {

		# Mark ambigous points
		data_den_sort <- t(apply(exp(data_den), 1, sort))
		data_den_ratio <- data_den_sort[,n_clusters]/data_den_sort[,n_clusters-1]

	 	ambig_rm <- data_den_ratio < den_ratio_cutoff	
		ambig_rm <- ambig_rm & fix_data$type != 'oob' & fix_data$type != 'part' & 
							  fix_data$type != 'nit'

		fix_data$type[ambig_rm] <- 'amb'

		# Mark points with very low density
		inv_dnorm <- function(x) {sqrt( -2*log(sqrt(2*pi) * x)) }
		density_rm <-
			inv_dnorm(exp(data_den_max)) > den_sd_cutoff

		# Old way to remove outliers
		#density_rm <- 
		#	data_den_max < (mean(data_den_max) - den_sd_cutoff*sd(data_den_max))

		density_rm <- density_rm & fix_data$type != 'oob' & fix_data$type != 'amb' & 
									fix_data$type != 'part' & fix_data$type != 'nit'

		fix_data$type[density_rm] <- 'den'

		# Category membership	
		cats <- apply(data_den, 1, which.max)	
		fix_data$cat <- cats

		# Reclassify ambiguous pts based on surrounding fixations
		if (use_run_rule) {
			
			# Get indices of ambiguous pts
			amb_ind <- which(fix_data$type == 'amb')

			# Go through each of these points
			for (i in amb_ind) { 

				# Go backwards to get category membership of previous keeper
				j = i - 1
				repeat { 			
					
					if (j <= 0)
						prev_cat = -1
					else if (fix_data$type[j] == 'keep') 
						prev_cat = fix_data$cat[j]						
					else if (fix_data$type[j] == 'oob')
						prev_cat = -1
					else if (fix_data$type[j] == 'den')
						prev_cat = -1
					else if (fix_data$type[j] == 'part')
						prev_cat = -1
					else if (fix_data$type[j] == 'nit')
						prev_cat = -1
					else if (fix_data$type[j] == 'amb') {
						j = j - 1
						next 
					}

					break

				}

				# Go forwards to get category membership of next keeper
				j = i + 1
				repeat { 			
					
					if (j > length(fix_data$type))
						next_cat = -1
					else if (fix_data$type[j] == 'keep') 
						next_cat = fix_data$cat[j]						
					else if (fix_data$type[j] == 'oob')
						next_cat = -1
					else if (fix_data$type[j] == 'den')
						next_cat = -1
					else if (fix_data$type[j] == 'part') # Shouldn't happen, but...
						next_cat = -1
					else if (fix_data$type[j] == 'nit') # Shouldn't happen, but...
						next_cat = -1
					else if (fix_data$type[j] == 'amb') {
						j = j + 1
						next 
					}

					break

				}

				# If both before and after are from the same category, reclassify
				if (prev_cat == next_cat && prev_cat != -1) {

					fix_data$type[i] = 'keep'
					fix_data$cat[i] = prev_cat		

				}

			}

			# Store the new category memberships
			cats <- fix_data$cat

		}

		# Store recategorized y-values
		if (keep_y_var)

			for (i in 1:length(cats))
				fix_data$y_new[i] <- ys[cats[i]] + y_diff[i,cats[i]]

		else
			fix_data$y_new <- ys[cats]

		# Store category membership, untransformed parameters, fit measure, fixation data
		line_ret <- list()

		line_ret$cats <- cats
		line_ret$params <- c(k, o, s)
		line_ret$fit_measure <- fit_measure
		line_ret$fix_data <- fix_data
		line_ret$start_pts <- start_pts

		# Return it
		return(line_ret)
	}

}

##############################
# do_trial_plots
##############################
do_trial_plots <- function(line_ret, t, sub_file, dur_five_num, show_image, 
											     save_trial_plots, fa_dir) {

	# Constants ----------------------------
	xy_buffer = .1
	pt_size_min = 1
	pt_size_max = 7

	# Handle data --------------------------

	# Separate out the fixation data
	fix_data_all <- line_ret$fix_data

	# Point sizes based on duration
	m <- (pt_size_max - pt_size_min)/(dur_five_num[5] - dur_five_num[1]) 
  fix_data_all$pt_size <- m*(fix_data_all$dur - dur_five_num[1]) + pt_size_min

	# Separate out some more data
	fix_data_keep <- fix_data_all[fix_data_all$type == 'keep',]
	cats_keep <- line_ret$cats[fix_data_all$type=='keep']
	fix_data_oob <-  fix_data_all[fix_data_all$type == 'oob',]
	fix_data_den <-  fix_data_all[fix_data_all$type == 'den',]
	fix_data_amb <-  fix_data_all[fix_data_all$type == 'amb',]

  # Plot parameters  ----------------------
  old.par <- par(no.readonly=TRUE)
  par(oma=c(0, 0, 3, 0))

  layout(c(1, 2))
  par(ask=TRUE)

	# x and y limits 
	x_min <- min(fix_data_all$x) - xy_buffer*(max(fix_data_all$x) - min(fix_data_all$x))
	x_max <- max(fix_data_all$x) + xy_buffer*(max(fix_data_all$x) - min(fix_data_all$x))

	y_min <- min(fix_data_all$y) - xy_buffer*(max(fix_data_all$y) - min(fix_data_all$y))
	y_max <- max(fix_data_all$y) + xy_buffer*(max(fix_data_all$y) - min(fix_data_all$y))

	# Line info ----------------------
	slope <- line_ret$params[1]
	vert_offset <- line_ret$params[2]

	start_pts <- line_ret$start_pts

	n_lines <- nrow(start_pts)

	# Handle background images -----------------
	if (show_image) {

		# Determine image to show

		# Is there a specific image for this subject and trial?
		image_file_name_trial <- paste(substr(sub_file, 1, nchar(x)-4), '_', t, '.tiff', sep='')

		if (file.exists(image_file_name_trial)) {

			# Load the image
			t_image <- readTiff(image_file_name_trial)	

		} else {
			
			# Get the subject specific image
			image_file_name_sub <- paste(substr(sub_file, 1, nchar(x)-4), '.tiff', sep='')

			if (file.exists(image_file_name_sub)) {

				# Load the image
				t_image <- readTiff(image_file_name_sub)

			} else {
			
				# If the image file is missing show the data without it
				warning(paste('Missing image for ', sub_file, ' trial ', t, '.', sep=''))
				show_image <- FALSE

			}

		}

		# Flip image upside-down
		t_image@red <- t_image@red[t_image@size[1]:0,]
		t_image@green <- t_image@green[t_image@size[1]:0,]
		t_image@blue <- t_image@blue[t_image@size[1]:0,]

		t_image_width = t_image@size[2]
		t_image_height = t_image@size[1]

	}

	# Make sure the directory exists
	if (save_trial_plots)
		if (!file.exists(paste(fa_dir, 'Trial_Plots', sep="/")))
			dir.create(paste(fa_dir, 'Trial_Plots', sep="/"))

  # Plot original data with deletions ----------------------

	if (save_trial_plots)
		tiff(file = paste(fa_dir, '/Trial_Plots/Orig_', 
				 gsub('.asc', '', gsub('/', '_', sub_file)), '_', t, '.tiff', sep=""), 
			width = 4267, height = 3200, units = "px", res = 800, pointsize=6) 

	if (show_image) {

		# If we're drawing an image
		plot(t_image, 
				 main='Original Fixations with Deletions & Classifications', 
				 xlab='x', ylab='y',
				 xlim=c(0, t_image_width), ylim=c(t_image_height, 0))
		axis(1)
		axis(2)

	} else { 

		# Blank plot 
		# Reverse y-limits so 0 at top
		plot(1, type='n',
				 main='Original Fixations with Deletions & Classifications', 
				 xlab='x', ylab='y', 
		 		 xlim=c(x_min, x_max), ylim=c(y_max, y_min))

	}

	# Lines for fixation ordering
	points(fix_data_all$x, fix_data_all$y, 
   	 		 cex=fix_data_all$pt_size, col='yellow', 
				 pch=1, type='l', lty='dashed')

	# The kept fixations
	for (i in 1:n_lines) {

		cat <- cats_keep == i

		# Alternate category colors
		if (i%%2 == 1)
			col = 'black'
		else
			col = 'green'
	
		points(fix_data_keep$x[cat], fix_data_keep$y[cat], 
			   	 cex=fix_data_keep$pt_size[cat], col=col, pch=1)
 
	}

	# The deleted fixations
  points(fix_data_oob$x, fix_data_oob$y, 
	       cex=fix_data_oob$pt_size, col='red', pch=1) 
  points(fix_data_den$x, fix_data_den$y, 
         cex=fix_data_den$pt_size, col='orange', pch=1)
  points(fix_data_amb$x, fix_data_amb$y, 
         cex=fix_data_amb$pt_size, col='purple', pch=1)

	# Show fitted lines
	for (i in 1:n_lines)
		lines(c(start_pts[i,1], x_max), 
					c(start_pts[i,2] + vert_offset, start_pts[i,2] + slope*(x_max-x_min) + vert_offset), 
					col='grey')

  legend(x='bottomleft', 
				 legend=c('Kept', 'Kept', 'Out-of-bounds', 'Low density', 'Ambiguous'), 
         pch=c(1, 1, 1, 1, 1), 
				 col=c('black', 'green', 'red', 'orange', 'purple'), 
				 bty='n')

	# Show line parameters
	legend(x='bottomright',
				 legend=c(paste('Slope =', round(line_ret$params[1], digits=3)),
				 					paste('VOffset =', round(line_ret$params[2], digits=3)),
				 					paste('SD =', round(line_ret$params[3], digits=3)),
				 					paste('Fit =', round(line_ret$fit_measure, digits=3))),
				 bty='n')

	if (save_trial_plots)
		dev.off()

  # Plot reformatted data ----------------------

	if (save_trial_plots)
		tiff(file = paste(fa_dir, '/Trial_Plots/Reform_', 
				 gsub('.asc', '', gsub('/', '_', sub_file)), '_', t, '.tiff', sep=""), 
			 width = 4267, height = 3200, units = "px", res = 800, pointsize=6) 

	if (show_image) {

		# If we're drawing an image
		plot(t_image, 
				 main='Reformatted Fixations', 
				 xlab='x', ylab='New y',
				 xlim=c(0, t_image_width), ylim=c(t_image_height, 0))
		axis(1)
		axis(2)

	} else { 
	 
		# Blank plot
		# Reverse y-limits so 0 at top
		plot(1, type='n',
				 main='Reformatted Fixations', 
				 xlab='x', ylab='New y', 
				 xlim=c(x_min, x_max), ylim=c(y_max, y_min))

	}
	
	# The fixations
	for (i in 1:n_lines) {

		cat <- cats_keep == i

		# Alternate category colors
		if (i%%2 == 1)
			col = 'black'
		else
			col = 'green'
	
		points(fix_data_keep$x[cat], fix_data_keep$y_new[cat], 
			   	 cex=fix_data_keep$pt_size[cat], col=col, pch=1)
 
	}

	# Show desired lines
	for (i in 1:n_lines)
		lines(c(start_pts[i,1], x_max), 
					c(start_pts[i,2], start_pts[i,2]), 
					col='grey')

	# Duration scale information
  legend(x='bottomleft', 
				 legend=c(paste(dur_five_num[1], 'ms'), 
									paste(dur_five_num[2], 'ms'), 
									paste(dur_five_num[3], 'ms'), 
									paste(dur_five_num[4], 'ms'), 
								  paste(dur_five_num[5], 'ms')), 
         pch=c(1, 1), 
				 pt.cex=c(m*(dur_five_num[1] - dur_five_num[1]) + pt_size_min, 
								  m*(dur_five_num[2] - dur_five_num[1]) + pt_size_min, 
								  m*(dur_five_num[3] - dur_five_num[1]) + pt_size_min, 
								  m*(dur_five_num[4] - dur_five_num[1]) + pt_size_min, 
								  m*(dur_five_num[5] - dur_five_num[1]) + pt_size_min), 
				 col=c('black', 'black'), 
				 bty='n')

  mtext(paste('File: ', sub_file, ', Trial: ', t, sep=''), outer=TRUE)

	if (save_trial_plots)
		dev.off()

}

##############################
# save_summary
##############################
save_summary <- function(line_ret, sub_file, out_file, n_trials) {
	
	for (t in 1:n_trials)
	{

		slope <- line_ret[[t]]$params[1]
		voffset <- line_ret[[t]]$params[2]
		sd <- line_ret[[t]]$params[3]

		fit <- line_ret[[t]]$fit_measure

		n_total_fix <- nrow(line_ret[[t]]$fix_data)
	
		count_table <- table(line_ret[[t]]$fix_data$type)

		n_keep <- length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'keep'])
		n_oob <- length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'oob'])
		n_amb <- length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'amb'])
		n_den <- length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'den'])
		n_nit <- length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'nit'])
		n_part <- length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'part'])

		cat(paste(sub_file, t, slope, voffset, sd, fit, n_total_fix, n_keep, 
						  n_oob, n_amb, n_den, n_nit, n_part, '\n', 
							sep=' '), 
			  file=out_file)

	}
}

##############################
# write_asc_file
##############################

write_asc_file <- function(line_ret, orig_asc_file_name, n_trials, fa_dir, start_flag, asc_data) {

	# Name and open the new asc file 
	# fa = (f)ix_(a)lign

	# Check if directory exists, if not, make
	if (!file.exists(fa_dir))
		dir.create(fa_dir)

	# Make the file name
	new_asc_file_name <- strsplit(orig_asc_file_name, '/')
	new_asc_file_name <- new_asc_file_name[[1]][length(new_asc_file_name[[1]])]
	new_asc_file_name <- gsub('.asc', '_fa.asc', new_asc_file_name)
	new_asc_file_name <- paste(fa_dir, '/', new_asc_file_name, sep='')
	out_file <- file(new_asc_file_name, 'w')		

	# Read in the file
	f <- readLines(orig_asc_file_name)

	# Find all of the EFIX lines after start_flag, put into 1 vector
	trial_id_start <- asc_data$trial_id_start
	trial_start <- asc_data$trial_start
	trial_end <- asc_data$trial_end

	efix_lines <- numeric(0)
	for (t in 1:n_trials) {

		trial_data  <- f[trial_id_start[t]:trial_end[t]]

		trial_end_fix <- which(grepl('EFIX', trial_data))
	
		efix_lines <- append(efix_lines, trial_end_fix + trial_id_start[t] - 1)
		
	}
	
	# Make a vector of all of the y-values in line_ret
	line_ret_y_vals <- numeric(0)
	for (t in 1:n_trials) {

		# Get a single trial of data
		line_ret_trial <- line_ret[[t]]

		# Mark the y-values for the deleted fixations
		line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'nit'] <- -1000
		line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'part'] <- -1001
		line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'oob'] <- -1002
		line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'amb'] <- -1003
		line_ret_trial$fix_data$y_new[line_ret_trial$fix_data$type == 'den'] <- -1004

		# Add the y-values onto the end of the vector
		line_ret_y_vals <- append(line_ret_y_vals, line_ret_trial$fix_data$y_new)

	}

	# Go through each fixation
	k <- 1
	for	(i in efix_lines) {

		# Parse out the fixation
		fix_data <- gsub('\\s+', ' ', f[i])
		fix_data <- strsplit(fix_data, ' ')	

		# What is the next reformatted y-coordinate 
		next_y <- line_ret_y_vals[k]

		# Change y-coordinate
		fix_data[[1]][7] <- next_y

		# Overwrite the original line
		f[i] <- paste(fix_data[[1]], sep='', collapse='   ')

		k <- k + 1

	}

	# Write the file
	#writeLines(f, out_file)
	writeLines(f, out_file, sep="\r\n") # Thanks to Brian Dillon for this suggestion

	# Close the file
	close(out_file)	

}


