import logging


def create_logger(logger_name: str) -> logging.Logger:
    """
    Function to create basic logger
    - Parameters:
        - logger_name
    """
    # Create or get the logger
    logger = logging.getLogger(logger_name)

    # Only add handlers if this logger has no handlers configured
    if not logger.handlers:
        # Create console handler with a specific format
        console_handler = logging.StreamHandler()
        formatter = logging.Formatter(
            "%(name)s - %(levelname)s - %(message)s"  # %(asctime)s - add this to beginninng of string to bring back timestamps
        )
        console_handler.setFormatter(formatter)

        # Add handler to logger
        logger.addHandler(console_handler)

        # Set default level
        logger.setLevel(logging.DEBUG)

    return logger
