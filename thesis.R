library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(tikzDevice)
library(RColorBrewer)
options(width = 260)

CPUS <- c(
    "Intel(R)_Xeon(R)_Platinum_8252C_CPU_@_3.80GHz",
    "Intel(R)_Xeon(R)_Platinum_8375C_CPU_@_2.90GHz",
    "Intel(R)_Core(TM)_i7-4790_CPU_@_3.60GHz",
    "AMD_Ryzen_7_3700X_8-Core_Processor",
    "Neoverse-N1",
    "Intel(R)_Xeon(R)_Gold_6354_CPU_@_3.00GHz"
)

MITIGATIONS <- c(
    "mitigations=off",
    "nopti%mds=off",
    "mitigations=auto"
)

MECHANISMS <- c(
    "ycall",
    "vdso",
    "fastcall",
    "syscall",
    "ioctl"
)

# Read the data
df.spec <- cols(
    name = col_character(),
    time_unit = col_character(),
    iterations = col_integer(),
    real_time = col_double(),
    cpu_time = col_double(),
    bytes_per_second = col_double(),
    items_per_second = col_double(),
    label = col_character(),
    error_occurred = col_integer(),
    error_message = col_character(),
)

df <- data.frame()
for (cpu in CPUS) {
    # Traverse the CPU directory
    cpu_dir <- file.path("results", cpu)
    for (mitigation in MITIGATIONS) {
        # Traverse the mitigation directory
        mitigation_dir <- file.path(cpu_dir, mitigation)
        # Traverse the mechanisms
        for (mechanism in MECHANISMS) {
            mechanism_path <- file.path(
                mitigation_dir, paste0(mechanism, ".csv")
            )

            # Skip if file does not exist
            if (!file.exists(mechanism_path)) {
                print(paste0("Skipping ", mechanism_path))
                next
            }

            df <- read_csv(mechanism_path, col_type = df.spec) %>%
                mutate(
                    cpu = cpu,
                    mitigation = mitigation,
                    mechanism = mechanism,
                    path = mechanism_path,
                ) %>%
                bind_rows(df)
        }
    }
}

df.noop <- df %>%
    filter(
        name != "fastcall_noop",
    ) %>%
    mutate(
        # Remove *Fixture*/ prefix
        name = str_replace(name, ".*Fixture[^/]*/", ""),
    ) %>%
    separate_wider_delim(
        name,
        names = c("Benchmark", "Size"), delim = "/", too_few = "align_start"
    ) %>%
    mutate(
        Benchmark = str_replace(
            str_replace(Benchmark, "examples_", ""), paste0(mechanism, "_"), ""
        ),
    ) %>%
    mutate(
        Benchmark = if_else(Benchmark == "sys_ni_syscall", "noop", Benchmark),
    ) %>%
    filter(
        Benchmark == "noop",
    ) %>%
    mutate(
        mechanism = fct_recode(mechanism,
            "vDSO" = "vdso",
            "FC (U)" = "ycall",
            "FC (P)" = "fastcall",
            "Syscall" = "syscall",
            #' IOCTL' = 'ioctl',
        ),
        mitigation = fct_recode(mitigation,
            "None" = "mitigations=off",
            "No KPTI and MDS" = "nopti%mds=off",
            "Default" = "mitigations=auto",
        ),
    )

order_factors <- function(x) {
    x %>% mutate(
        # Reorder mitigations
        mitigation = factor(
            mitigation,
            levels = c("None", "No KPTI and MDS", "Default")
        ),
        # Reorder mechanisms
        mechanism = factor(
            mechanism,
            levels = c("vDSO", "FC (U)", "FC (P)", "Syscall", "ioctl")
        ),
    )
}


cur.cpu <- "Intel(R)_Core(TM)_i7-4790_CPU_@_3.60GHz"
df.ffmk <- df.noop %>%
    filter(
        cpu == cur.cpu,
    ) %>%
    filter(
        mitigation != "No KPTI and MDS"
    )

df.ffmk.label <- data.frame(
    mechanism = c("Syscall", "Syscall"),
    mitigation = c("None", "Default"),
    label_y = c(300, 400),
    label_x = c(1, 2)
)

# Make a barplot
p <- df.ffmk.label %>%
    right_join(df.ffmk, copy = TRUE) %>%
    mutate(
        real_time = if_else(
            real_time < 10, round(real_time, 1), round(real_time, 0)
        )
    ) %>%
    mutate(
        # Reorder mitigations
        mitigation = factor(
            mitigation,
            levels = c("None", "No KPTI and MDS", "Default")
        ),
        # Reorder mechanisms
        mechanism = factor(
            mechanism,
            levels = c("vDSO", "FC (U)", "FC (P)", "Syscall", "ioctl")
        ),
    ) %>%
    ggplot(aes(x = mechanism, y = real_time, fill = mitigation)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_brewer(
        type = "qual", palette = "Dark2", name = "Mitigations", direction = -1
    ) +
    geom_text(aes(label = real_time),
        position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5
    ) +
    # geom_segment(
    #     aes(
    #         xend = label_x, yend = label_y,
    #         x = mechanism, y = real_time,
    #     ),
    #     position = position_dodge(width = 0.9),
    #     alpha = 0.5,
    #     arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed")
    # ) +
    # geom_label(
    #     aes(
    #         label = mitigation,
    #         x = label_x, y = label_y
    #     ),
    #     size = 2.5, fill = "white", color = "black",
    #     label.size = 0, label.padding = unit(0.1, "lines"), na.rm = TRUE
    # ) +
    ylab("Time (ns)") +
    xlab("Mechanism") +
    expand_limits(y = 420) +
    theme_pubr(base_size = 8) %+replace%
    theme(axis.text = element_text(size = 7))
plot(p)

# Save the plot as tikz
tikz(
    file.path("plots", cur.cpu, "latency", "latency.tex"),
    width = 2.3, height = 2.5
)
plot(p)
dev.off()




cur.cpu <- "Intel(R)_Xeon(R)_Gold_6354_CPU_@_3.00GHz"
df.oci <- df.noop %>%
    filter(
        cpu == cur.cpu,
    )

my_palette <- brewer.pal(name = "Dark2", n = 3)[2:3]

# Make a barplot
p <- df.oci %>%
    mutate(
        real_time = if_else(
            real_time < 10, round(real_time, 1), round(real_time, 0)
        )
    ) %>%
    order_factors() %>%
    filter(
        mitigation != "No KPTI and MDS",
        mitigation == "Default"
    ) %>%
    ggplot(aes(x = mechanism, y = real_time, fill = mitigation)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(
        values = my_palette,
        name = "Mitigations"
        # type = "qual", palette = "Dark2", direction = 1
    ) +
    geom_text(aes(label = real_time),
        position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5
    ) +
    # geom_segment(
    #     aes(
    #         xend = label_x, yend = label_y,
    #         x = mechanism, y = real_time,
    #     ),
    #     position = position_dodge(width = 0.9),
    #     alpha = 0.5,
    #     arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed")
    # ) +
    # geom_label(
    #     aes(
    #         label = mitigation,
    #         x = label_x, y = label_y
    #     ),
    #     size = 2.5, fill = "white", color = "black",
    #     label.size = 0, label.padding = unit(0.1, "lines"), na.rm = TRUE
    # ) +
    ylab("Time (ns)") +
    xlab("Mechanism") +
    expand_limits(y = 90) +
    theme_pubr(base_size = 8) %+replace%
    theme(
        axis.text = element_text(size = 7),
        legend.position = "none"
    )
plot(p)

# Save the plot as tikz
tikz(
    file.path("plots", cur.cpu, "latency", "latency.tex"),
    width = 2.3, height = 2.2
)
plot(p)
dev.off()

df.cpu.freq <- data.frame(
    cpu = c(
        "Intel(R)_Xeon(R)_Platinum_8252C_CPU_@_3.80GHz",
        "Intel(R)_Xeon(R)_Platinum_8375C_CPU_@_2.90GHz",
        "Intel(R)_Core(TM)_i7-4790_CPU_@_3.60GHz",
        "AMD_Ryzen_7_3700X_8-Core_Processor",
        "Neoverse-N1",
        "Intel(R)_Xeon(R)_Gold_6354_CPU_@_3.00GHz"
    ),
    short_cpu = c(
        "8252C",
        "8375C",
        "i7-4790",
        "3700X",
        "Neoverse-N1",
        "6354"
    ),
    vulnerable = c(
        "Vulnerable",
        "Not vulnerable",
        "Vulnerable",
        "Not vulnerable",
        "Not vulnerable",
        "Not vulnerable"
    ),
    freq = c(
        3800,
        2900,
        3600,
        3600,
        2700,
        3000
    )
)
p <- df.noop %>%
    filter(
        str_count(cpu, "Intel") > 0,
        mitigation == "None",
    ) %>%
    left_join(df.cpu.freq, by = "cpu") %>%
    mutate(
        cycles = real_time * freq / 1000,
        vulnerable = factor(
            vulnerable,
            levels = c("Vulnerable", "Not vulnerable")
        )
    ) %>%
    arrange(cycles) %>%
    filter(mechanism == "FC (P)") %>%
    # ggplot(aes(x = cpu, y = real_time, fill = mitigation)) +
    ggplot(aes(
        x = fct_reorder(short_cpu, cycles),
        y = cycles, fill = vulnerable
    )) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_brewer(
        type = "qual",
        palette = "Dark2", name = "Mitigations", direction = -1
    ) +
    ylab("Cycles") +
    xlab("CPU") +
    theme_pubr(base_size = 8) %+replace%
    theme(
        strip.background = element_blank(),
        strip.placement = "outside", legend.position = "none"
    ) +
    facet_wrap(~vulnerable, scales = "free_x")
plot(p)
# Save the plot as tikz
tikz(
    file.path("plots", "vulnerabilities.tex"),
    width = 2.3, height = 2.5
)
plot(p)
dev.off()


# Read the data
df.spec <- cols(
    noop = col_integer(),
    `ycall-registration-minimal` = col_integer(),
    `ycall-deregistration-minimal` = col_integer(),
    `fork-simple` = col_integer(),
    `fork-ycall` = col_integer(),
    `vfork-simple` = col_integer(),
    `vfork-ycall` = col_integer(),
    `registration-minimal` = col_integer(),
    `registration-mappings` = col_integer(),
    `deregistration-minimal` = col_integer(),
    `deregistration-mappings` = col_integer(),
    `fork-fastcall` = col_integer(),
    `vfork-simple` = col_integer(),
    `vfork-fastcall` = col_integer()
)

df.all <- data.frame()
for (cpu in CPUS) {
    # Traverse the CPU directory
    cpu_dir <- file.path("results", cpu)
    for (mitigation in MITIGATIONS) {
        # Traverse the mitigation directory
        mitigation_dir <- file.path(cpu_dir, mitigation)
        # Traverse the mechanisms
        for (mechanism in MECHANISMS) {
            mechanism_path <- file.path(
                mitigation_dir, paste0("misc-", mechanism, ".csv")
            )

            # Skip if file does not exist
            if (!file.exists(mechanism_path)) {
                print(paste0("Skipping ", mechanism_path))
                next
            }
            print(paste0("Reading ", mechanism_path))

            df.all <- read_csv(mechanism_path, col_type = df.spec) %>%
                pivot_longer(
                    cols = everything(),
                    names_to = "measurement",
                    values_to = "time"
                ) %>%
                mutate(
                    cpu = cpu,
                    mitigation = mitigation,
                    mechanism = mechanism,
                    path = mechanism_path,
                ) %>%
                bind_rows(df.all)
        }
    }
}

df <- df.all %>%
    group_by(
        cpu, mitigation, mechanism, measurement
    ) %>%
    filter(
        time < quantile(time, 0.99),
    ) %>%
    summarise(
        time.sd = sd(time),
        time = median(time),
        n = n(),
    )

df.reg <- df %>%
    filter(
        measurement %in% c(
            "registration-minimal",
            "ycall-registration-minimal",
            "deregistration-minimal",
            "ycall-deregistration-minimal"
        ),
        mitigation == "mitigations=auto",
        cpu == "Intel(R)_Xeon(R)_Gold_6354_CPU_@_3.00GHz"
    )
p <- df.reg %>%
    ungroup() %>%
    mutate(
        measurement = str_to_title(
            str_replace(
                str_replace(measurement, "ycall-", ""), "-minimal", ""
            )
        ),
    ) %>%
    mutate(
        mechanism = fct_recode(mechanism,
            "Unpriv." = "ycall",
            "Priv." = "fastcall",
        ),
        measurement = fct_reorder(measurement, time),
    ) %>%
    left_join(df.cpu.freq, by = "cpu") %>%
    arrange(time) %>%
    ggplot(aes(
        x = fct_reorder(mechanism, time),
        y = time / 1000, fill = mechanism
    )) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(
        aes(ymin = (time - time.sd) / 1000, ymax = (time + time.sd) / 1000),
        position = position_dodge(width = 0.9), width = 0.2
    ) +
    scale_fill_brewer(
        type = "qual",
        palette = "Dark2", name = "Mitigations", direction = -1
    ) +
    ylab("Time (us)") +
    xlab("Operation") +
    theme_pubr(base_size = 8) %+replace%
    theme(
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none",
        axis.text = element_text(size = 7)
    ) +
    facet_wrap(~measurement, scales = "free_x")
plot(p)

# Save the plot as tikz
tikz(
    file.path("plots", cur.cpu, "reg-dereg.tex"),
    width = 2.3, height = 2.5
)
plot(p)
dev.off()

df.fork <-
    df %>%
    ungroup() %>%
    filter(
        str_count(measurement, "fork") > 0,
        str_count(measurement, "vfork") == 0,
        mitigation == "mitigations=auto",
        # mechanism == "fastcall",
        cpu == cur.cpu
    ) %>%
    separate_wider_delim(
        measurement,
        names = c("fork", "base"), delim = "-"
    ) %>%
    select(-mitigation, -time.sd) %>%
    mutate(base = if_else(base == "ycall", "fastcall", base)) %>%
    group_by(cpu, mechanism, fork) %>%
    pivot_wider(
        names_from = base,
        values_from = time
    ) %>%
    left_join(df.cpu.freq, by = "cpu") %>%
    mutate(
        fastcall = fastcall - simple,
    ) %>%
    pivot_longer(
        cols = c("fastcall", "simple"),
        names_to = "run",
        values_to = "time"
    )
# mutate(
#     overhead = time_fastcall - time_simple,
#     overhead.cycles = overhead * freq / 1000,
# ) %>%

df.label <- data.frame(
    x = c(1.45, 2),
    y = c(43, 42),
    xend = c("Unprivileged", "Unprivileged"),
    yend = c(45, 45)
)

p <- df.fork %>%
    ungroup() %>%
    mutate(
        mechanism = fct_recode(mechanism,
            "Unprivileged" = "ycall",
            "Privileged" = "fastcall",
        ),
    ) %>%
    ggplot(aes(
        x = mechanism,
        y = time / 1000, fill = run
    )) +
    geom_col() +
    geom_segment(
        data = df.label,
        aes(x = x, y = y, xend = xend, yend = yend),
        alpha = 0.5,
        arrow = arrow(length = unit(0.10, "cm"), ends = "first", type = "closed"),
        inherit.aes = FALSE
    ) +
    geom_text(
        data = df.label %>% head(n = 1),
        aes(x = xend, y = yend, label = "With fastcalls"),
        position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5,
        inherit.aes = FALSE
    ) +
    scale_fill_brewer(
        type = "qual",
        palette = "Dark2", name = "Mitigations", direction = -1
    ) +
    expand_limits(y = 47) +
    ylab("Time (us)") +
    xlab("Fastcall") +
    theme_pubr(base_size = 8) %+replace%
    theme(
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none",
        axis.text = element_text(size = 7)
    )
plot(p)

# Save the plot as tikz
tikz(
    file.path("plots", cur.cpu, "fork.tex"),
    width = 1.8, height = 2
)
plot(p)
dev.off()

df.noop %>%
    filter(
        mitigation == "Default" | cpu == "Neoverse-N1"
    ) %>%
    select(
        -bytes_per_second,
        -items_per_second,
        -error_occurred,
        -error_message,
        -Size,
        -Benchmark,
        -time_unit,
        -path,
        -mitigation,
        -label,
        -cpu_time,
        -iterations
    ) %>% 
    left_join(df.cpu.freq, by = "cpu") %>%
    mutate(
    #    real_time = real_time * freq / 1000,
    ) %>%
    pivot_wider(
        names_from = mechanism,
        values_from = real_time
    ) %>%
    select(
        short_cpu, freq, vDSO, `FC (U)`, `FC (P)`, Syscall, ioctl
    ) %>%
    mutate(
        i = c(5, 4, 3, 1, 6, 2)
    ) %>%
    #filter(short_cpu != "Neoverse-N1") %>%
    arrange(i) %>%
    mutate(ioctl / Syscall) 

UNITS <- c("misc", "cycles")
KERNELS <- c("fastcall", "fccmp", "ycall")
MITIGATIONS <- c("mitigations=off")
df <- data.frame()
for (cpu in CPUS) {
    # Traverse the CPU directory
    cpu_dir <- file.path("results", cpu)
    for (mitigation in MITIGATIONS) {
        # Traverse the mitigation directory
        mitigation_dir <- file.path(cpu_dir, mitigation)
        # Traverse the kernels
        for (kernel in KERNELS) {
            for (unit in UNITS) {
                mechanism_path <- file.path(
                    mitigation_dir, paste0(unit, "-", kernel, ".csv")
                )

                # Skip if file does not exist
                if (!file.exists(mechanism_path)) {
                    print(paste0("Skipping ", mechanism_path))
                    next
                }

                df <- read_csv(mechanism_path, col_type = df.spec) %>%
                    mutate(
                        cpu = cpu,
                        mitigation = mitigation,
                        kernel = kernel,
                        path = mechanism_path,
                        i = row_number(),
                        unit = if_else(
                            unit == "misc", "ns", "cycles"
                        )
                    ) %>%
                    bind_rows(df)
            }
        }
    }
}

#df.noop 
df %>%
    select(
        -`ycall-registration-minimal`,
        -`ycall-deregistration-minimal`,
        -`fork-simple`,
        -`fork-fastcall`,
        -`fork-ycall`,
        -`vfork-simple`,
        -`vfork-fastcall`,
        -`vfork-ycall`,
        -`registration-minimal`,
        -`registration-mappings`,
        -`deregistration-minimal`,
        -`deregistration-mappings`,
    ) %>%
    pivot_longer(
        cols = c(
            `noop`,
            `ycall`,
            `vdso`,
            `syscall`,
            `ioctl`,
            `fastcall`
        ),
        names_to = "mechanism",
        values_to = "time"
    ) %>%
    # mutate(
    #     old_mechanism = mechanism,
    #     mechanism = if_else(mechanism == "noop", kernel, mechanism),
    # ) %>%
    filter(
        !is.na(time),
        # i == 1
    ) %>% 
    group_by(
        cpu, mitigation, kernel,
        mechanism,
        unit
    ) %>%
    summarise(
        n = n(),
        # time.sd = sd(time),
        time = median(time),
    ) %>%
    ungroup() %>% filter(unit == "cycles") %>%
    filter(
        !(mechanism == "noop" & kernel == "fastcall"),
        !(mechanism == "noop" & kernel == "ycall")
    ) %>%
    select(
        -n,
        -kernel
    ) %>%
    pivot_wider(
        names_from = mechanism,
        values_from = time
    ) %>%
    mutate(
        fastcall_d = fastcall - noop,
        ycall_d = ycall - noop,
        ioctl_d = ioctl - noop,
        syscall_d = syscall - noop,
        vdso_d = vdso - noop
    )

    select(
        -n,
        -kernel
    ) %>%
    pivot_wider(
        names_from = mechanism,
        values_from = time
    ) %>%
    arrange(unit, cpu, kernel)

read_csv("cycle_stats.csv") %>% 
    filter(mitigation == "mitigations=off") %>%
    group_by(CPU, mitigation, kernel, benchmark) %>%
    select(adjusted, median) %>%
    pivot_wider(
        names_from = benchmark,
        values_from = median
    ) %>% print(n = Inf)
    mutate(
        i = row_number()
    ) %>%
    filter(
        i %in% c(3897, 3898, 3899, 6216, 6217, 6218)
    )
    pivot_longer(
        cols = everything(),
        names_to = "measurement",
        values_to = "time"
    ) %>%
    drop_na()
    arrange(-time)

